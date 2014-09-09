{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts,
             TypeSynonymInstances, MultiParamTypeClasses,
             OverloadedLists, TypeFamilies,
             FunctionalDependencies, FlexibleInstances #-}
module Language.Rowling.TypeCheck.TypeChecker (
  module Language.Rowling.Definitions.Types,
  typeIt, typeIt', loadParam, typeWithContext, typeExpr, typeExprN
  ) where

import qualified Prelude as P
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Data.ContextStack

import Language.Rowling.Common hiding (only)
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Parser
import Language.Rowling.Definitions.Types
import Language.Rowling.TypeCheck.Builtins

-- | Our monadic state.
data TCState = TCState {
  _count :: Int, -- For generating new names.
  _typeMaps :: [TypeMap], -- Maps names to types.
  _typeAliases :: AliasMap -- Stores type aliases.
} deriving (Show)

-- | The TCState is a stack, where the frames are type maps.
instance Stack TCState where
  type Frame TCState = TypeMap
  push tmap state = state {_typeMaps = push tmap $ _typeMaps state}
  pop state = (top, state {_typeMaps=rest}) where
    top:rest = _typeMaps state
  asList = _typeMaps
  modifyTop func state = state {_typeMaps=func top : rest} where
    top:rest = _typeMaps state

instance Default TCState where
  def = TCState {
    _count=0,
    _typeMaps=[defaultTypeMap],
    _typeAliases=defaultTypeAliases
  }

-- | The main type checking monad.
type TypeChecker = ExceptT ErrorList (StateT TCState Identity)

-- | A mapping from (type variable) names to BaseTypes.
newtype Substitution = Substitution (HashMap Name Type) deriving (Show, Eq)

instance Render Substitution where
  render (Substitution s) = "{" <> T.intercalate ", " items <> "}" where
    items = fmap (\(f,t) -> render f <> "=>" <> render t) $ H.toList s

class Substitutable a where (•>) :: Substitution -> a -> a
instance Substitutable Type where
  subs@(Substitution s) •> t = case t of
    TVar n | H.member n s -> s H.! n
    TRecord fields Nothing -> TRecord (fmap (subs •>) fields) Nothing
    TRecord fields (Just r) -> case lookup r s of
      Nothing -> TRecord (fmap (subs •>) fields) (Just r)
      Just (TRecord fields' r') -> TRecord (fields' <> fields) r'
      Just _ -> P.error "Not a row type"
    TApply t1 t2 -> TApply (subs •> t1) (subs •> t2)
    _ -> t

instance Substitutable Polytype where
  subs •> (Polytype vars t) = (Polytype vars (s t)) where
    s = (S.foldr' remove subs vars •>)

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  subs •> (a, b) = (subs •> a, subs •> b)

instance (Substitutable val) => Substitutable (HashMap key val) where
  subs •> m = fmap (subs •>) m

instance Substitutable a => Substitutable [a] where
  subs •> list = fmap (subs •>) list

-- | Composes two substitutions. The newer substitution should go second.
(•) :: Substitution -> Substitution -> Substitution
Substitution s1 • Substitution s2 = Substitution (s1' <> s2) where
  s1' = fmap (Substitution s2 •>) s1

-- | Composes a list of substitutions.
compose :: [Substitution] -> Substitution
compose = foldl' (•) noSubs

-- | A singleton substitution.
oneSub :: Name -> Type -> Substitution
oneSub n = Substitution . H.singleton n

-- | Removes a name from a substitution.
remove :: Name -> Substitution -> Substitution
remove n (Substitution s) = Substitution (H.delete n s)

noSubs :: Substitution
noSubs = Substitution mempty

-- | Replaces all quanified variables in the polytype with fresh variables.
instantiate :: Polytype -> TypeChecker Type
instantiate (Polytype vars t) = do
  -- Make a substitution from all variables owned by this polytype
  sub <- fmap compose $ forM (S.toList vars) $ \v -> oneSub v <$> newvar
  -- Apply the subtitution to the owned type
  return $ sub •> t

-- | Find which variables in the type are completely "owned" by the type
-- (i.e., are not found in the surrounding environment) and creates a polytype
-- with all of those variables quantified.
generalize :: Type -> TypeChecker Polytype
generalize t = do freeFromEnv <- freevars . _typeMaps <$> get
                  return $ Polytype (freevars t \\ freeFromEnv) t

-- | Applies a substitution to the environment in the monad.
substituteEnv :: Substitution -> TypeChecker ()
substituteEnv subs = do
  modify $ \s -> s {_typeMaps = subs •> _typeMaps s}

-- | Given two types, produces a substitution which if applied would make the
-- two types equal. Throws an error if such a substitution is impossible.
unify :: Type -> Type -> TypeChecker Substitution
unify t1 t2 = case (t1, t2) of
  (TVar a, _) -> return (Substitution (H.singleton a t2))
  (_, TVar a) -> return (Substitution (H.singleton a t1))
  (TConst n1, TConst n2) | n1 == n2 -> return noSubs
  (TApply a1 a2, TApply b1 b2) -> do
    s1 <- unify a1 b1
    s2 <- unify (s1 •> a2) (s1 •> b2)
    return (s1 • s2)
  (TRecord f1 r1, TRecord f2 r2) -> unifyRows (f1, r1) (f2, r2)
  (_, _) -> throwErrorC ["Can't unify types ", render t1, " and ", render t2]

-- | Unifies the fields and "rest" of two row types.
unifyRows :: (HashMap Name Type, Maybe Name)
          -> (HashMap Name Type, Maybe Name)
          -> TypeChecker Substitution
unifyRows (f1, r1) (f2, r2) = do
  let presentInBoth = keys f1 `L.intersect` keys f2
  pSubs <- fmap compose $ forM presentInBoth $ \field ->
    unify (f1 ! field) (f2 ! field)
  subsR <- getAbsent (keys f1 L.\\ presentInBoth) r2
  subsL <- getAbsent (keys f2 L.\\ presentInBoth) r1
  return $ pSubs • subsR • subsL
  where
    getAbsent :: [Name] -> Maybe Name -> TypeChecker Substitution
    getAbsent absent rest = if L.null absent then return noSubs else
      case rest of
        Nothing -> throwErrorC ["Missing fields ", render absent]
        Just name -> do TVar rest' <- newvar
                        let fields = H.fromList [(k, f1 ! k) | k <- absent]
                        return (oneSub name $ TRecord fields $ Just rest')

-- | Produces a fresh type variable.
newvar :: TypeChecker Type
newvar = do i <- gets _count
            modify $ \s -> s {_count = i + 1}
            return $ TVar $ "$t" <> render i

-- | Produces two new variables.
newvar2 :: TypeChecker (Type, Type)
newvar2 = (,) <$> newvar <*> newvar

defaultTypeAliases :: HashMap Name Type
defaultTypeAliases = mempty

defaultTypeMap :: TypeMap
defaultTypeMap = builtInTypes

-- | Infers the type of an expression.
typeOf :: Expr -> TypeChecker (Type, Substitution)
typeOf expr = go expr `catchError` whenTyping where
  go = \case
    Int _ -> only "Int"
    Float _ -> only "Float"
    Constructor "True" -> only "Bool"
    Constructor "False" -> only "Bool"
    String _ -> only "String"
    Variable name -> findOrError' name >>= instantiate >>= only where
      findOrError' = findOrError $
        ErrorList ["No variable '", name, "' in scope"]
    Lambda param body -> withFrame mempty $ do
      (paramT, paramS) <- loadParam param
      (bodyT, bodyS) <- typeOf body
      applyAndReturn (paramT ==> bodyT) (paramS • bodyS)
    Apply e1 e2 -> do
      (t1, s1) <- typeOf' e1
      (t2, s2) <- typeOf e2
      resultT <- newvar
      s3 <- unify t1 (t2 ==> resultT) `catchError` addErrorC
        [renderTicks e1, " has type ", renderTicks $ s1 • s2 •> t1, " and ",
         renderTicks e2, " has type ", renderTicks $ s1 • s2 •> t2]
      applyAndReturn resultT (s1 • s2 • s3)
    Let name e1 e2 -> do
      (t1, s1) <- typeOf' e1
      pt <- generalize t1
      withBindings ([(name, pt)] :: [(Name, Polytype)]) $ typeOf e2
    Dot expr field -> do
      (t, s) <- typeOf expr
      (t', s') <- deref t field
      applyAndReturn t' (s • s')
    Record fields -> do
      (ts, subs) <- typeOfPairList typeOf $ H.toList fields
      return (TRecord (H.fromList ts) Nothing, subs)
    Typed expr typ -> do
      (t, s) <- typeOf expr
      s' <- unify t typ
      applyAndReturn t (s • s')
    List list -> typeOfList typeOf $ toList list

  whenTyping = addErrorC ["When typing the expression `", render expr, "`"]

applyAndReturn :: Type -> Substitution -> TypeChecker (Type, Substitution)
applyAndReturn t s = return (s •> t, s)

typeOfPairList :: (Expr -> TypeChecker (Type, Substitution))
               -> [(Name, Expr)] -> TypeChecker ([(Name, Type)], Substitution)
typeOfPairList typeOf = go noSubs where
  go subs [] = return ([], subs)
  go subs ((name, expr):others) = do
    substituteEnv subs
    (t, s) <- typeOf expr
    (ts, s') <- go (subs • s) others
    return ((name, t) : ts, subs • s • s')

typesInList :: (Expr -> TypeChecker (Type, Substitution))
            -> [Expr] -> TypeChecker ([Type], Substitution)
typesInList typeOf = go noSubs where
  go subs [] = return ([], subs)
  go subs (expr:others) = do
    substituteEnv subs
    (t, s) <- typeOf expr
    (ts, s') <- go (subs • s) others
    return (t : ts, subs • s • s')

typeOfList :: (Expr -> TypeChecker (Type, Substitution))
           -> [Expr] -> TypeChecker (Type, Substitution)
typeOfList typeOf exprs = do
  -- Start off with a list of a generic type.
  res <- newvar
  -- Get all the types in the list, and then unify them all.
  typesInList typeOf exprs >>= go res where
    go result ([], subs) = applyAndReturn (TApply "List" result) subs
    go result ((t:ts), subs) = do
      subs' <- unify t result
      go result (ts, subs • subs')

-- | Shortcut for returning a type with no substitutions.
only :: Type -> TypeChecker (Type, Substitution)
only t = return (t, noSubs)

-- | Loads new bindings from a pattern. So a variable doesn't get looked up;
-- rather it gets added as a new variable to the scope.
loadParam :: Expr -> TypeChecker (Type, Substitution)
loadParam = \case
  Variable name -> do var <- newvar
                      store name $ polytype var
                      only var
  Record fields -> do
    TVar rest <- newvar
    (fieldTypes, subs) <- runStateT (mapM go fields) noSubs
    return (TRecord fieldTypes (Just rest), subs)
    where go :: Expr -> StateT Substitution TypeChecker Type
          go expr = do
            (t, subs) <- lift $ loadParam expr
            modify (subs •)
            return t

  Typed expr t -> do (t', s) <- loadParam expr
                     s' <- unify t' t
                     applyAndReturn t (s • s')
  p -> throwErrorC ["Invalid lambda argument: ", render p]

asTuple :: HashMap Name Expr -> TypeChecker [Expr]
asTuple fields | H.null fields = return []
asTuple fields = check >> enumerate (0 :: Int) where
  check = forM_ (H.keys fields) $ \k -> do
    let msg = "Function arguments must be variables or tuples"
    when (not $ T.all isDigit k) (throwError1 msg)
  enumerate i = case H.lookup (render i) fields of
    Nothing -> return []
    Just e -> (e :) <$> enumerate (i + 1)

-- | Converts a list of types into a "tuple" (record with integer fields).
toTupleType :: [Type] -> Type
toTupleType ts = TRecord ts' Nothing where
  ts' = H.fromList $ zip (map render [0 :: Int ..]) ts

-- | Gets the type and applies the substitutions to the environment in one go.
typeOf' :: Expr -> TypeChecker (Type, Substitution)
typeOf' expr = typeOf expr >>= \(t, s) -> substituteEnv s >> return (t, s)

-- | Resolves a type alias.
resolveAlias :: Name -> TypeChecker (Maybe Type)
resolveAlias alias = lookup alias <$> gets _typeAliases

-- | Looks up a field in a type.
deref :: Type -> Name -> TypeChecker (Type, Substitution)
deref t field = case t of
  TRecord fields rest -> case (lookup field fields, rest) of
    (Just t', _) -> return (t', noSubs)
    (Nothing, Just rest) -> do
      (var, TVar rest') <- newvar2
      return (var, oneSub rest $ TRecord (H.singleton field var) (Just rest'))
    (Nothing, Nothing) -> noFieldErr
  TVar name -> do
    (var, TVar rest) <- newvar2
    return (var, oneSub name $ TRecord (H.singleton field var) (Just rest))
  TConst name -> resolveAlias name >>= \case
    Just t' -> deref t' field `ifErrorDo` noFieldErr
    Nothing -> noFieldErr
  _ -> noFieldErr
  where
    noFieldErr = throwErrorC ["Type ", render t, " has no field '", field, "'"]

-- | Runs the type checking monad with a default state.
runTyping :: TypeChecker a -> Either ErrorList a
runTyping = fst . runTypingWith def

-- | Runs the type checking monad with a given state.
runTypingWith :: TCState -> TypeChecker a -> (Either ErrorList a, TCState)
runTypingWith state action = do
  runIdentity $ runStateT (runExceptT action) state

-- | Get the type of an expression, or error.
typeExpr :: Expr -> Either ErrorList Type
typeExpr = typeWithBindings empty

-- | Get the type of an expression, normalized, or error.
typeExprN :: Expr -> Either ErrorList Type
typeExprN = fmap normalize . typeExpr

-- | Get the type of an expression with some environment, or error.
typeWithBindings :: TypeMap -> Expr -> Either ErrorList Type
typeWithBindings bindings expr = do
  let state :: TCState
      state = def {_typeMaps = [bindings, builtInTypes]}
  case fst $ runTypingWith state $ typeOf expr of
    Right (t, s) -> return $ s •> t
    Left err -> Left err


typeIt :: P.String -> (Either ErrorList (Type, Substitution), TCState)
typeIt = typeIt' typeOf

typeIt' :: (Expr -> TypeChecker a)
        -> P.String -> (Either ErrorList a, TCState)
typeIt' typer input = case parseIt input of
  Left err -> (Left $ ErrorList [pack $ show err], def)
  Right expr -> runTypingWith def $ typer expr

typeWith :: MonadError ErrorList m
             => TCState -> Expr -> m (Type, TCState)
typeWith state expr = case runTypingWith state (typeOf expr) of
  (Left el, _) -> throwError el
  (Right (t, subs), state') -> return (subs •> t, state')

typeWithContext :: (Functor m, MonadError ErrorList m)
                => TypeMap -> AliasMap -> Expr -> m (Type, TypeMap)
typeWithContext tmap amap expr = do
  let state = TCState {_count = 0, _typeMaps = [tmap], _typeAliases = amap}
  (t, state) <- typeWith state expr
  return (t, P.head $ _typeMaps state)
