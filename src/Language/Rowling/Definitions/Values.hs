{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Rowling.Definitions.Values where

import Data.ContextStack
import qualified GHC.Exts as GHC

import Language.Rowling.Common
import Language.Rowling.Definitions.Expressions
import qualified Data.HashMap.Strict as H

-- | The evaluated form of an @Expr@.
data Value = VInt Integer                  -- ^ An integer.
           | VFloat Double                 -- ^ A floating-point number.
           | VString Text                  -- ^ A string.
           | VBool Bool                    -- ^ A boolean.
           | VList (Vector Value)          -- ^ A list of values.
           | VTaggedUnion Name (Seq Value) -- ^ A tagged union, like a Maybe.
           | VBuiltin Builtin              -- ^ A builtin function.
           | VRecord (HashMap Name Value)  -- ^ An instantiated Record (Model).
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

instance IsList Value where
  type Item Value = Value
  fromList = VList . GHC.fromList
  toList (VList vs) = GHC.toList vs
  toList _ = error "Not a list value"

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
