{-# LANGUAGE OverloadedStrings, LambdaCase,
             RecordWildCards, OverloadedLists,
             FlexibleContexts, NoMonomorphismRestriction #-}
module Language.Rowling.Evaluator.Builtins (builtins) where

import qualified Prelude as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.String (IsString(..))

import Language.Rowling.Common
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Definitions.Values
import Language.Rowling.Evaluator.Evaluator

builtins :: HashMap Name Value
builtins =
  [
    ("+", builtinBinaryNum "+" (+) (+)),
    ("-", builtinBinaryNum "-" (-) (-)),
    ("*", builtinBinaryNum "*" (*) (*)),
    ("/", builtinBinaryNum "/" div (/)),
    (">", builtinBinaryNumComp ">" (>) (>)),
    ("<", builtinBinaryNumComp "<" (<) (<)),
    (">=", builtinBinaryNumComp ">=" (>=) (>=)),
    ("<=", builtinBinaryNumComp "<=" (<=) (<=)),
    ("==", builtinBinaryNumComp "==" (==) (==)),
    ("!=", builtinBinaryNumComp "!=" (/=) (/=)),
    ("&&", VBuiltin $ builtinBinaryNumBool "and" (&&)),
    ("||", VBuiltin $ builtinBinaryNumBool "or" (||)),
    ("not", VBuiltin builtinNot),
    ("each", VBuiltin builtinEach)
  ]

-- | Shorthand for wrapping a function as a builtin.
bi :: Name -> (Value -> Eval Value) -> Value
bi name = VBuiltin . Builtin name

-- | Takes a numeric operator for integers and for doubles, and makes a
-- builtin of it.
builtinBinaryNum :: Name -- ^ The name of the builtin
                 -> (Integer -> Integer -> Integer) -- ^ Integer operator
                 -> (Double -> Double -> Double) -- ^ Floating point operator
                 -> Value -- ^ A builtin value
builtinBinaryNum name i2i f2f = bi name $ \case
  VInt n -> return $! builtinInt n
  VFloat f -> return $! builtinFloat f
  _ -> error "Not a number"
  where builtinInt n = bi (render n <> name) $ \case
          VInt n' -> return $! VInt $ n `i2i` n'
          VFloat f -> return $! VFloat $ fromInteger n `f2f` f
          _ -> error "Not a number"
        builtinFloat f = bi (render f <> name) $ \case
          VInt n -> return $ VFloat $ f `f2f` fromInteger n
          VFloat f' -> return $ VFloat $ f `f2f` f'
          _ -> error "Not a number"

builtinBinaryNumComp :: Name -> (Integer -> Integer -> Bool)
                     -> (Double -> Double -> Bool)
                    b  -> Value
builtinBinaryNumComp name i2i f2f = bi name $ \case
  VInt n -> return $ builtinInt n
  VFloat f -> return $ builtinFloat f
  _ -> error "Not a number"
  where builtinInt n = bi (render n <> name) $ \case
          VInt n' -> return $ VBool $ n `i2i` n'
          VFloat f -> return $ VBool $ fromInteger n `f2f` f
          _ -> error "Not a number"
        builtinFloat f = bi (render f <> name) $ \case
          VInt n -> return $ VBool $ f `f2f` fromInteger n
          VFloat f' -> return $ VBool $ f `f2f` f'
          _ -> error "Not a number"

builtinBinaryNumBool :: Name -> (Bool -> Bool -> Bool) -> Builtin
builtinBinaryNumBool name op = Builtin name $ \case
  VBool b -> return $ bi (name <> "(" <> render b <> ")") $ \case
    VBool b' -> return $ VBool $ b `op` b'
    _ -> error "Not a bool"
  _ -> error "Not a bool"

builtinNot :: Builtin
builtinNot = Builtin "not" $ \case
  VBool b -> return $ VBool (not b)
  _ -> error "Not a bool"

builtinEach :: Builtin
builtinEach = Builtin "each" $ \case
  VArray vals -> return $! eachVals vals
  _ -> error "Not a list"
  where
    eachVals vals = bi "eachApplied" $ \func -> do
      let applyFunc val = case func of
            VClosure{..} -> undefined
            VBuiltin (Builtin _ builtin) -> builtin val
      results <- mapM applyFunc vals
      return $ VArray results
