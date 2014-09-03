{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleInstances,
             OverloadedLists, TypeSynonymInstances #-}
module Language.Rowling.Definitions.Types where

import qualified Prelude as P
import Data.Char (isLower)
import qualified Data.HashMap.Strict as H
import Data.Text (Text, strip, head, length)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.List as L
import Data.String (IsString(..))
import Language.Rowling.Common hiding (head, length)
import Data.Traversable

data Type = TRecord (HashMap Name Type) (Maybe Name)
          | TConst Name
          | TVar Name
          | TApply Type Type
          deriving (Show, Eq)

instance Render Type where
  render t = case t of
    TConst name -> name
    TVar name -> name
    TApply (TApply (TConst "->") t1) t2 ->
      renderParens t1 <> " -> " <> render t2
    TApply (t1@(TApply _ _)) t2 -> render t1 <> " " <> renderParens t2
    TApply t1 t2 -> renderParens t1 <> " " <> renderParens t2
    TRecord row r -> "(" <> inside <> renderRest r <> ")" where
      inside = case splitRow row of
        ([], named) -> commas renderPair named
        (vals, []) -> commas render vals
        (vals, named) -> commas render vals <> ", " <> commas renderPair named
    where
      renderRest r = case r of
        Nothing -> ""
        Just name -> " | " <> name
      commas rndr = intercalate ", " . map rndr
      renderPair (field, typ) = field <> ": " <> render typ
      -- | Splits a row into the fields which have numeric names (0, 1, ...)
      -- and the ones that have "normal" names.
      splitRow row = (snd <$> nums', withNames) where
        (withNums, withNames) = L.partition (isNumber . fst) $ H.toList row
        toNum (name, t) = (P.read (unpack name) :: Int, t)
        nums' = L.sortBy (\(a, _) (b, _) -> compare a b) (map toNum withNums)
  renderParens t@(TApply _ _) = "(" <> render t <> ")"
  renderParens t = render t

instance Default Type where
  def = TRecord mempty Nothing

isNumber :: Text -> Bool
isNumber = T.all isDigit

instance IsString Type where
  fromString s = do
    let s' = strip $ pack s
    if length s' > 0 && (isLower (head s') || head s' == '$')
    then TVar s'
    else TConst s'

instance FuzzyEquals Type where
  TRecord fields1 r1 ~~ TRecord fields2 r2 =
    fields1 ~~ fields2 && case (r1, r2) of
      (Nothing, Nothing) -> True
      (Just _, _) -> True
      _ -> False
  TConst name1 ~~ TConst name2 = name1 == name2
  TVar _ ~~ _ = True
  TApply t1 t2 ~~ TApply t3 t4 = t1 ~~ t3 && t2 ~~ t4

-- | Class of things which contain free variables. @freevars@ gets all of the
--  free variables out of a type. For example the type @a@ has free variables
-- {a}; the type @a -> b@ has free variables {a, b}; the type @Maybe (a ->
-- Int) -> b -> b@ has free variables {a, b}, etc.
class FreeVars a where freevars :: a -> Set Name
instance FreeVars Type where
  freevars = \case
    TVar n -> S.singleton n
    TConst _ -> mempty
    TApply t1 t2 -> freevars t1 <> freevars t2
    TRecord fields Nothing -> freevars fields
    TRecord fields (Just r) -> S.insert r $ mconcat (fmap freevars $ toList fields)
instance FreeVars Polytype where
  freevars (Polytype vars t) = freevars t \\ vars
instance FreeVars a => FreeVars (Maybe a) where
  freevars Nothing = mempty
  freevars (Just x) = freevars x
instance FreeVars b => FreeVars (a, b) where
  freevars = freevars . snd
instance FreeVars a => FreeVars [a] where
  freevars = mconcat . fmap freevars
instance FreeVars a => FreeVars (HashMap x a) where
  freevars = freevars . H.elems

data Polytype = Polytype (Set Name) Type deriving (Show, Eq)

instance IsString Polytype where
  fromString = Polytype mempty . fromString

-- | Stores names that we've typed.
type TypeMap = HashMap Name Polytype

instance Default TypeMap where
  def = mempty

-- | Stores type aliases.
type AliasMap = HashMap Name Type

instance Default AliasMap where
  def = mempty

-- | Normalizing means replacing obscurely-named type variables with letters.
-- For example, the type @(t$13 -> [t$4]) -> t$13@ would be @(a -> b) -> a@.
-- The best way to do this is with a state monad so that we can track which
-- renamings have been done. So the only method that we need is @normalizeS@
-- (@S@ for state monad). This lets us normalize across multiple types.
class Normalize t where
  normalizeS :: t -> State (Text, HashMap Name Name) t

normalize :: Normalize a => a -> a
normalize = normalizeWith ("a", mempty)

normalizeWith :: Normalize a => (Text, HashMap Name Name) -> a -> a
normalizeWith state x = evalState (normalizeS x) state

instance Normalize Type where
  normalizeS type_ = case type_ of
    TVar name -> TVar <$> normalizeS name
    TApply a b -> TApply <$> normalizeS a <*> normalizeS b
    TRecord row rest -> TRecord <$> normalizeS row <*> normalizeS rest
    _ -> return type_

next :: Text -> Text
next name = case T.last name of
  c | c < 'z' -> T.init name `T.snoc` succ c
    | True    -> name `T.snoc` 'a'

instance (Normalize a, Traversable f) => Normalize (f a) where
  normalizeS = mapM normalizeS

instance Normalize Text where
  normalizeS oldName = do
    (newName, mapping) <- get
    case H.lookup oldName mapping of
      Just n -> return n
      Nothing -> do put (next newName, H.insert oldName newName mapping)
                    return newName

class CanApply a where apply :: a -> a -> a
instance CanApply Type where
  apply = TApply
instance CanApply Polytype where
  apply (Polytype vs1 t1) (Polytype vs2 t2) =
    Polytype (vs1 <> vs2) (apply t1 t2)

-- | The function type, which is actually a rank-2 type applied twice.
(==>) :: (IsString a, CanApply a) => a -> a -> a
t1 ==> t2 = apply (apply "->" t1) t2

infixr 3 ==>

polytype :: Type -> Polytype
polytype = Polytype mempty

tRecord :: [(Name, Type)] -> Type
tRecord fields = TRecord (H.fromList fields) Nothing