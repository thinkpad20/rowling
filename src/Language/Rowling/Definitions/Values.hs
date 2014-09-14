{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Describes Rowling values, i.e., the entities which are produced by
-- evaluating expressions, and passed as input to the evaluator.
module Language.Rowling.Definitions.Values where

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object)
import qualified Data.Aeson as Aeson
import Data.ContextStack
import qualified Data.HashMap.Strict as H
import Data.Scientific (isInteger, toRealFloat, fromFloatDigits)
import qualified GHC.Exts as GHC

import Language.Rowling.Common
import Language.Rowling.Definitions.Expressions

-- | The evaluated form of an `Expr`.
data Value = VInt !Integer                  -- ^ An integer.
           | VFloat !Double                 -- ^ A floating-point number.
           | VString !Text                  -- ^ A string.
           | VBool !Bool                    -- ^ A boolean.
           | VArray !(Vector Value)         -- ^ An array of values.
           | VTagged !Name !(Vector Value)
           -- ^ A tagged union, like `Either` or `List`.
           | VMaybe !(Maybe Value)
           -- ^ A maybe (using the Haskell type for efficiency)
           | VBuiltin !Builtin              -- ^ A builtin function.
           | VRecord !(Record Value) -- ^ An instantiated Record.
           | VClosure !(Record Value) !Pattern !Expr -- ^ A closure.
           deriving (Show, Eq)

-- | Looks up a field in a record. If the field doesn't exist, or the value
-- isn't a record, an IO exception will be thrown.
deref :: Name -> Value -> Value
deref name (VRecord fields) = case H.lookup name fields of
  Nothing -> error $ "No field " <> show name
  Just val -> val
deref _ _ = error "Not a record value"

instance Render Value

-- | The default value is an empty record, akin to @()@.
instance Default Value where
  def = VRecord mempty

-- | Makes creating string values more convenient.
instance IsString Value where
  fromString = VString . fromString

-- | Array values are lists, that contain values.
instance IsList Value where
  type Item Value = Value
  fromList = VArray . GHC.fromList
  toList (VArray vs) = GHC.toList vs
  toList _ = error "Not a list value"

-- | Values can be read out of JSON. Of course, closures and builtins
-- can't be represented.
instance FromJSON Value where
  parseJSON (Aeson.Object v) = VRecord <$> mapM parseJSON v
  parseJSON (Aeson.Array arr) = VArray <$> mapM parseJSON arr
  parseJSON (Aeson.String s) = return $ VString s
  parseJSON (Aeson.Bool b) = return $ VBool b
  parseJSON (Aeson.Number n)
    | isInteger n = return $ VInt (floor n)
    | otherwise   = return $ VFloat $ toRealFloat n
  parseJSON _ = mzero


-- | Most values have JSON representations. Where they don't, it's an error
-- to try to serialize them to JSON.
instance ToJSON Value where
  toJSON (VInt i) = Aeson.Number $ fromIntegral i
  toJSON (VFloat f) = Aeson.Number $ fromFloatDigits f
  toJSON (VString txt) = Aeson.String txt
  toJSON (VBool b) = Aeson.Bool b
  toJSON (VArray arr) = Aeson.Array $ map toJSON arr
  toJSON (VMaybe Nothing) = object ["@constructor" .= Aeson.String "None"]
  toJSON (VMaybe (Just v)) = object [
    "@constructor" .= Aeson.String "Some",
    "@values" .= Aeson.Array [toJSON v]
    ]
  toJSON (VTagged name vals) = object ["@constructor" .= name,
                                       "@values" .= map toJSON vals]
  toJSON (VRecord rec) = Aeson.Object $ map toJSON rec
  toJSON v = errorC ["Can't serialize '", render v, "' to JSON"]

-- | Matches a pattern against a value and either fails, or returns a map of
-- name bindings. For example, matching the pattern @Just x@ against the value
-- @VTagged "Just" (VInt 1)@ would return @[("x", VInt 1)]@. Matching the
-- same pattern against @VFloat 2.3@ would return @Nothing@.
patternMatch :: Pattern -> Value -> Maybe (Record Value)
patternMatch p v = case (p, v) of
-- Primitive literals just match if equal. (Constructors are literals).
  (Int n, VInt vn) | n == vn -> Just mempty
  (Float n, VFloat vn) | n == vn -> Just mempty
  (String (Plain s), VString vs) | s == vs -> Just mempty
  -- True and false are constructors, but use Haskell bools
  (Constructor "True", VBool True) -> Just mempty
  (Constructor "False", VBool False) -> Just mempty
  (Constructor "None", VMaybe Nothing) -> Just mempty
  (Apply (Constructor "Some") p, VMaybe (Just v)) -> patternMatch p v
  -- A variable can match with anything.
  (Variable name, v) -> Just [(name, v)]
  -- With a list expression, it matches if and only if all of them match.
  (List ps, VArray vs) -> matchVector ps vs
  (Record precord, VRecord vrecord) -> matchRecord precord vrecord
  -- For a compound expression, dive into it (see below).
  (compoundExpr, VTagged n' vs) -> case dive compoundExpr of
    Just (n, ps) | n == n' -> matchVector (fromList ps) vs
    otherwise -> Nothing
  -- Anything else is not a match.
  otherwise -> Nothing
  where
    matchRecord precord vrecord = loop mempty $ H.toList precord where
      loop bindings [] = Just bindings
      loop bindings ((key, pattern):rest) = case lookup key vrecord of
        -- Key doesn't exist, pattern match fails.
        Nothing -> Nothing
        Just val -> case patternMatch pattern val of
          -- Value doesn't pattern match with pattern, no match.
          Nothing -> Nothing
          Just bindings' -> loop (bindings' <> bindings) rest
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

-- | An evaluation frame. It consists of the argument passed into the
-- function currently being evaluated, and all of the variables in the
-- current scope.
data EvalFrame = EvalFrame {
  _fArgument :: Value,
  _fEnvironment :: Record Value
} deriving (Show)

-- | A frame is a key-value store where the internal dictionary is the
-- environment.
instance KeyValueStore EvalFrame where
  type LookupKey EvalFrame = Name
  type StoredValue EvalFrame = Value
  empty = def
  loadBindings bs f = f {_fEnvironment = bs <> _fEnvironment f}
  getValue name = lookup name . _fEnvironment
  putValue name val frame = frame {_fEnvironment = insertMap name val env}
    where env = _fEnvironment frame

-- | The evaluator's state is a stack of evaluation frames.
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

-- | The default evaluation state is a stack with a single evaluation frame.
instance Default EvalState where
  def = EvalState {_esStack = [def]}

-- | The default evaluation frame just takes default arguments and
-- environment.
instance Default EvalFrame where
  def = EvalFrame {_fArgument = def, _fEnvironment = mempty}

-- | The evaluator monad.
type Eval = ReaderT () (StateT EvalState IO)

-- | A built-in function. Allows us to write functions in Haskell and
-- make them callable from inside the Rowling evaluator.
data Builtin = Builtin Name (Value -> Eval Value)

-- | All we show is the function's name, which is assumed to be unique.
instance Show Builtin where
  show (Builtin n _) = "<BUILTIN " <> show n <> ">"

-- | Builtins are considered equal if they have the same name.
instance Eq Builtin where
  Builtin n1 _ == Builtin n2 _ = n1 == n2
