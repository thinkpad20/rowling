{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts,
             TypeSynonymInstances, MultiParamTypeClasses,
             OverloadedLists, TypeFamilies,
             FunctionalDependencies, FlexibleInstances #-}
module Language.Rowling.TypeCheck.TypeChecker (
  module Language.Rowling.Definitions.Types,
  typeIt, typeIt', typeOfPattern, typeWithContext, typeExpr, typeExprN,
  pTypeIt, typeWithBindingsN
  ) where

import qualified Prelude as P
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Data.ContextStack
import System.IO.Unsafe

import Language.Rowling.Common hiding (only)
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Parser
import Language.Rowling.Definitions.Types
import Language.Rowling.TypeCheck.Builtins

-- | The state of the type checker.
data TCState = TCState {
  _count :: Int, -- ^ For generating new names.
  _typeMaps :: [TypeMap], -- ^ Maps names to types.
  _typeAliases :: AliasMap -- ^ Stores type aliases.
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

-- The default type checker state contains the built-in type map.
instance Default TCState where
  def = TCState {
    _count=0,
    _typeMaps=[builtInTypes],
    _typeAliases=def
  }

-- | The main type checking monad, containing the type checker state.
type TypeChecker = ExceptT ErrorList (StateT TCState IO)

------------------------------------------------------------------------------
-- * Substitutions
------------------------------------------------------------------------------

-- | A mapping from (type variable) names to BaseTypes.
newtype Substitution = Substitution (HashMap Name Type) deriving (Show, Eq)

-- | Substitutions render as @{name => type}@.
instance Render Substitution where
  render (Substitution s) = "{" <> T.intercalate ", " items <> "}" where
    items = fmap (\(f,t) -> render f <> "=>" <> render t) $ H.toList s

-- | A class for things to which a substitution can be applied.
class Substitutable a where (•>) :: Substitution -> a -> a

-- | A substitution applied to a type replaces all variables in the type that
-- appear in the substitution.
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

-- | Similar to a substitution, but first we remove all of the variables
-- that are bound in the polytype.
instance Substitutable Polytype where
  subs •> (Polytype vars t) = (Polytype vars (subs' •> t)) where
    subs' = S.foldr' remove subs vars

-- | When substituting into a tuple, just apply the substitutions to both.
instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  subs •> (a, b) = (subs •> a, subs •> b)

-- | Apply the substitutions to all values.
instance (Substitutable val) => Substitutable (HashMap key val) where
  subs •> m = fmap (subs •>) m

-- | Apply the substitution to each member of the list.
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

-- | An empty substitution.
noSubs :: Substitution
noSubs = Substitution mempty

-- | Applies a substitution to the environment in the monad.
substituteEnv :: Substitution -> TypeChecker ()
substituteEnv subs = do
  modify $ \s -> s {_typeMaps = subs •> _typeMaps s}

------------------------------------------------------------------------------
-- * Instantiation and Generalization
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- * Unification
------------------------------------------------------------------------------

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

-- | Unifies all of the types in a list.
unifyAll :: [Type] -> TypeChecker Substitution
unifyAll [] = return noSubs
unifyAll [t] = return noSubs
unifyAll (t1:t2:ts) = do s <- unify t1 t2
                         s' <- unifyAll (t2:ts)
                         return (s • s')

------------------------------------------------------------------------------
-- * Type Inferrence Functions
------------------------------------------------------------------------------

-- ** Main Type Inferrence Function

-- | Infers the type of an expression. Mostly follows Algorithm W.
typeOf :: Expr -> TypeChecker (Type, Substitution)
typeOf expr = go expr `catchError` whenTyping where
  go = \case
    Int _ -> only "Int"
    Float _ -> only "Float"
    Constructor "True" -> only "Bool"
    Constructor "False" -> only "Bool"
    String _ -> only "String"
    Constructor c -> find >>= instantiate >>= only where
      find = findOrError (oneErrorC ["Unknown constructor ", tshow c]) c
    Variable name -> find name >>= instantiate >>= only where
      find = findOrError $ oneErrorC ["No variable '" <> name <> "' in scope"]
    Lambda name body -> withFrame mempty $ do
      paramT <- newvar
      store name $ polytype paramT
      (bodyT, bodyS) <- typeOf body
      applyAndReturn (paramT ==> bodyT) bodyS
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
    Case test alts -> do
      (testT, s1) <- typeOf test
      (resultT, resultS) <- typeOfAlts (s1 •> testT) alts
      applyAndReturn resultT (s1 • resultS)
    e -> errorC ["Cannot type the expression ", renderTicks e]
  whenTyping = addErrorC ["When typing the expression `", render expr, "`"]

-- ** Type checkers specialized for certain scenarios
-- Many of these functions take a function as an argument; this is so that
-- they can be used for various different type checking functions.

-- | Gets the type of a list of alternatives (pairs of pattern -> result). The
-- list must not be empty, and the types of all patterns and all results must
-- match each other (or at least be able to be unified with each other).
typeOfAlts :: Type -- ^ The type that each pattern should have.
           -> [(Pattern, Expr)] -- ^ The alternatives list.
           -> TypeChecker (Type, Substitution) -- ^ The result type and all
                                               -- collected substitutions.
typeOfAlts testT [] = throwError1 "No alternatives in case expression"
typeOfAlts testT ((pattern, result):rest) = do
  ((resultType, patternType), subs) <- withFrame empty $ do
    (patternT, patternS) <- typeOfPattern pattern
    unifyS <- unify patternT testT
    (resultT, resultS) <- typeOf result
    applyAndReturn (resultT, patternT) (patternS • unifyS • resultS)
  case rest of
    [] -> return (resultType, subs)
    _  -> do (resultType', subs') <- typeOfAlts patternType rest
             unifyS <- unify resultType (subs •> resultType')
             applyAndReturn resultType (subs • subs')

-- | Gets the types of all expressions in a list of @(Name, Expr)@ tuples.
typeOfPairList :: (Expr -> TypeChecker (Type, Substitution))
               -> [(Name, Expr)] -> TypeChecker ([(Name, Type)], Substitution)
typeOfPairList typeOf = go noSubs where
  go subs [] = return ([], subs)
  go subs ((name, expr):others) = do
    substituteEnv subs
    (t, s) <- typeOf expr
    (ts, s') <- go (subs • s) others
    return ((name, t) : ts, subs • s • s')

-- | Gets the types of all expressions in a list. The types do not need to be
-- the same.
typesInList :: (Expr -> TypeChecker (Type, Substitution))
            -> [Expr] -> TypeChecker ([Type], Substitution)
typesInList typeOf = go noSubs where
  go subs [] = return ([], subs)
  go subs (expr:others) = do
    substituteEnv subs
    (t, s) <- typeOf expr
    (ts, s') <- go (subs • s) others
    return (t : ts, subs • s • s')

-- | Gets the type of expressions in a list. All of these types must be equal.
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
      go (subs • subs' •> result) (ts, subs • subs')

-- | Loads new bindings from a pattern. So a variable doesn't get looked up;
-- rather it gets added as a new variable to the scope.
typeOfPattern :: Pattern -> TypeChecker (Type, Substitution)
typeOfPattern pattern = case pattern of
  -- Constant patterns are valid, and don't generate any bindings.
  Int _ -> only "Int"
  Float _ -> only "Float"
  Constructor "True" -> only "Bool"
  Constructor "False" -> only "Bool"
  String _ -> only "String"
  -- With a variable, we assign it a fresh type.
  Variable name -> do var <- newvar
                      store name $ polytype var
                      only var
  -- The "rest" variable of a field gets instantiated here.
  Record fields -> do
    TVar rest <- newvar
    (fieldTypes, subs) <- runStateT (mapM go fields) noSubs
    return (TRecord fieldTypes (Just rest), subs)
    where go :: Pattern -> StateT Substitution TypeChecker Type
          go expr = do
            (t, subs) <- lift $ typeOfPattern expr
            modify (subs •)
            return t
  -- Typed patterns let us assert that the pattern follows a particular type.
  Typed expr t -> do (t', s) <- typeOfPattern expr `ifErrorDo` invalid
                     s' <- unify t' t
                     applyAndReturn t (s • s')
  -- Lists can appear in patterns.
  List patterns -> typeOfList typeOfPattern (toList patterns)
  -- Any other valid pattern will be a constructed expression, built from
  -- applying some constructor to 0 or more arguments. We can separate these
  -- out by calling `unroll`.
  _ -> case unroll pattern of
    (Constructor c, args) -> do
      let find = findOrError $ oneErrorC ["Unknown constructor ", tshow c]
      constructorT <- instantiate =<< find c
      constructedT <- newvar
      (argTs, argSubs) <- typesInList typeOfPattern args
      unifySubs <- unify constructorT (foldr (==>) constructedT argTs)
      applyAndReturn constructedT (argSubs • unifySubs)
    _ -> invalid
  where invalid = throwErrorC ["Invalid pattern: ", render pattern]


-- ** Helper functions


-- | Gets the type and applies the substitutions to the environment in one go.
typeOf' :: Expr -> TypeChecker (Type, Substitution)
typeOf' expr = typeOf expr >>= \(t, s) -> substituteEnv s >> return (t, s)

-- | Produces a fresh type variable.
newvar :: TypeChecker Type
newvar = do i <- gets _count
            modify $ \s -> s {_count = i + 1}
            return $ TVar $ "$t" <> render i

-- | Applies its second argument to its first, and then returns both.
applyAndReturn :: Substitutable a
               => a -> Substitution -> TypeChecker (a, Substitution)
applyAndReturn t s = return (s •> t, s)

-- | Shortcut for returning a type with no substitutions.
only :: Type -> TypeChecker (Type, Substitution)
only t = return (t, noSubs)

-- | Resolves a type alias.
resolveAlias :: Name -> TypeChecker (Maybe Type)
resolveAlias alias = lookup alias <$> gets _typeAliases

-- | Looks up a field in a type.
deref :: Type -> Name -> TypeChecker (Type, Substitution)
deref t field = case t of
  TRecord fields rest -> case (lookup field fields, rest) of
    (Just t', _) -> return (t', noSubs)
    (Nothing, Just rest) -> do
      (var, TVar rest') <- tuple newvar newvar
      return (var, oneSub rest $ TRecord (H.singleton field var) (Just rest'))
    (Nothing, Nothing) -> noFieldErr
  TVar name -> do
    (var, TVar rest) <- tuple newvar newvar
    return (var, oneSub name $ TRecord (H.singleton field var) (Just rest))
  TConst name -> resolveAlias name >>= \case
    Just t' -> deref t' field `ifErrorDo` noFieldErr
    Nothing -> noFieldErr
  _ -> noFieldErr
  where
    noFieldErr = throwErrorC ["Type ", render t, " has no field ",
                              render field]

------------------------------------------------------------------------------
-- * Monadic functions
------------------------------------------------------------------------------

-- | Runs the type checking monad with a default state.
runTyping :: TypeChecker a -> Either ErrorList a
runTyping = fst . runTypingWith def

-- | Runs the type checking monad with a given state.
runTypingWith :: TCState -> TypeChecker a -> (Either ErrorList a, TCState)
runTypingWith state action = do
  unsafePerformIO $ runStateT (runExceptT action) state

-- | Get the type of an expression, or error.
typeExpr :: Expr -> Either ErrorList Type
typeExpr = typeWithBindings empty

-- | Get the type of an expression, normalized, or error.
typeExprN :: Expr -> Either ErrorList Type
typeExprN = fmap normalize . typeExpr

-- | Get the type of an expression with some environment, or error.
typeWithBindings :: TypeMap -> Expr -> Either ErrorList Type
typeWithBindings bindings expr = do
  let state = def {_typeMaps = [bindings, builtInTypes]}
  case fst $ runTypingWith state $ typeOf expr of
    Right (t, s) -> return $ s •> t
    Left err -> Left err

-- | Get the type of an expression with some environment, normalized, or
-- error.
typeWithBindingsN :: TypeMap -> Expr -> Either ErrorList Type
typeWithBindingsN bindings = fmap normalize . typeWithBindings bindings

-- | Parses a string and gets its type.
typeIt :: P.String -> (Either ErrorList (Type, Substitution), TCState)
typeIt = typeIt' typeOf

-- | Parses a string and runs it into an arbitrary typechecker function.
typeIt' :: (Expr -> TypeChecker a)
        -> P.String -> (Either ErrorList a, TCState)
typeIt' typer input = case parseIt input of
  Left err -> (Left $ ErrorList (pack $ show err) [], def)
  Right expr -> runTypingWith def $ typer expr

-- | Runs the type checker with an initial state.
typeWith :: MonadError ErrorList m
         => TCState -> Expr -> m (Type, TCState)
typeWith state expr = case runTypingWith state (typeOf expr) of
  (Left el, _) -> throwError el
  (Right (t, subs), state') -> return (subs •> t, state')

-- | Runs the type checker given type mappings and type aliases.
typeWithContext :: (Functor m, MonadError ErrorList m)
                => TypeMap -> AliasMap -> Expr -> m (Type, TypeMap)
typeWithContext tmap amap expr = do
  let state = TCState {_count = 0, _typeMaps = [tmap], _typeAliases = amap}
  (t, state) <- typeWith state expr
  return (t, P.head $ _typeMaps state)

-- | Types the expression in the string and prints it.
pTypeIt :: P.String -> IO ()
pTypeIt input = case parseIt input of
  Left err -> error $ show err
  Right expr -> case typeExprN expr of
    Left errlist -> printErrors errlist
    Right type_ -> putStrLn $ render expr <> " : " <> render type_
