{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Rowling.Definitions.Values where

import Data.ContextStack
import qualified GHC.Exts as GHC

import Language.Rowling.Common
import Language.Rowling.Definitions.Expressions
import qualified Data.HashMap.Strict as H

-- | The evaluated form of an `Expr`.
data Value = VInt Integer                  -- ^ An integer.
           | VFloat Double                 -- ^ A floating-point number.
           | VString Text                  -- ^ A string.
           | VBool Bool                    -- ^ A boolean.
           | VList (Vector Value)          -- ^ A list of values.
           | VTagged Name (Vector Value)
           -- ^ A tagged union, like a Maybe.
           | VBuiltin Builtin              -- ^ A builtin function.
           | VRecord (HashMap Name Value)
           -- ^ An instantiated Record (Model).
           | VClosure {_cEnvironment :: EvalEnv,
                       _cBody :: Expr}
           -- ^ A function closure.
           deriving (Show, Eq)

deref :: Name -> Value -> Value
deref name (VRecord fields) = case H.lookup name fields of
  Nothing -> error $ "No field " <> show name
  Just val -> val
deref _ _ = error "Not a record value"

instance Render Value
instance Default Value where
  def = VRecord mempty

instance IsString Value where
  fromString = VString . fromString

instance IsList Value where
  type Item Value = Value
  fromList = VList . GHC.fromList
  toList (VList vs) = GHC.toList vs
  toList _ = error "Not a list value"

-- | Matches a pattern against a value and either fails, or returns a map of
-- name bindings. For example, matching the pattern @Just x@ against the value
-- @VTagged "Just" (VInt 1)@ would return @[("x", VInt 1)]@. Matching the
-- same pattern against @VFloat 2.3@ would return @Nothing@.
patternMatch :: Pattern -> Value -> Maybe (HashMap Name Value)
patternMatch p v = case (p, v) of
-- Primitive literals just match if equal. (Constructors are literals).
  (Int n, VInt vn) | n == vn -> Just mempty
  (Float n, VFloat vn) | n == vn -> Just mempty
  (String (Plain s), VString vs) | s == vs -> Just mempty
  -- | True and false are constructors
  (Constructor "True", VBool True) -> Just mempty
  (Constructor "False", VBool False) -> Just mempty
  -- A variable can match with anything.
  (Variable name, v) -> Just [(name, v)]
  -- With a list expression, it matches if and only if all of them match.
  (List ps, VList vs) -> matchVector ps vs
  -- For a compound expression, dive into it (see below).
  (compoundExpr, VTagged n' vs) -> case dive compoundExpr of
    Just (n, ps) | n == n' -> matchVector (fromList ps) vs
    otherwise -> Nothing
  -- Anything else is not a match.
  otherwise -> Nothing
  where
    matchVector ps vs = case length ps == length vs of
      True -> concat <$> mapM (uncurry patternMatch) (zip ps vs)
      False -> Nothing
    -- "Dives" into a pattern and grabs the constructor name and patterns.
    -- Note that it's only going to return a @Just@ value if the "left most"
    -- expression in the pattern is a constructor. This prevents an pattern
    -- like @a b@ from matching against @Just 1@.
    dive = map (map reverse) . dive' where
      dive' (Constructor n) = Just (n, [])
      dive' (Apply p1 p2) = for (dive' p1) (\(name, ps) -> (name, p2:ps))
      dive' _ = Nothing


------------------------------------------------------------------------------
-- * The Evaluator Monad
-- Note: we have to put these definitions here because `Value`s need to be
-- aware of the `Eval` type.
------------------------------------------------------------------------------

data EvalFrame = EvalFrame {
  _fArgument :: Value,
  _fEnvironment :: EvalEnv
} deriving (Show)

instance KeyValueStore EvalFrame where
  type LookupKey EvalFrame = Name
  type StoredValue EvalFrame = ValOrArg
  empty = def
  getValue name = lookup name . _fEnvironment
  putValue name val frame = frame {_fEnvironment = insertMap name val env}
    where env = _fEnvironment frame

data EvalState = EvalState {_esStack :: [EvalFrame]} deriving (Show)

-- | The evaluator state is a stack of `EvalFrame`s.
instance Stack EvalState where
  type Frame EvalState = EvalFrame
  push frame state = state {_esStack = push frame $ _esStack state}
  pop state = (top, state {_esStack=rest}) where
    top:rest = _esStack state
  asList = _esStack
  modifyTop func state = state {_esStack=func top : rest} where
    top:rest = _esStack state

instance Default EvalState where
  def = EvalState {_esStack = [def]}

instance Default EvalFrame where
  def = EvalFrame {_fArgument = def, _fEnvironment = mempty}

type Eval = ReaderT () (StateT EvalState IO)
type EvalEnv = HashMap Name ValOrArg
data ValOrArg = Arg
              | Val Value
              | VDot ValOrArg Name
              deriving (Show, Eq)

data Builtin = Builtin Name (Value -> Eval Value)

instance Show Builtin where
  show (Builtin n _) = unpack n <> "(builtin function)"

instance Eq Builtin where
  Builtin n1 _ == Builtin n2 _ = n1 == n2
