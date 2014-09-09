{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards,
             FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Rowling.Evaluator.Evaluator where

import qualified Prelude as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.ContextStack

import Language.Rowling.Common
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Definitions.Values

-- | Evaluates an expression into a value.
eval :: Expr -> Eval Value
eval !expr = case expr of
  Int i -> return $ VInt i
  Float f -> return $ VFloat f
  String interp -> go interp where
    go (Plain s) = return $ VString s
    go (Interp in1 e in2) = do
      VString s1 <- go in1
      e' <- render <$> eval e
      VString s2 <- go in2
      return $ VString $ s1 <> e' <> s2
  Constructor "True" -> return $ VBool True
  Constructor "False" -> return $ VBool False
  Variable var -> lookupNameOrError var
  Typed expr _ -> eval expr
  Lambda param body -> do
    env <- getClosure expr
    return $ VClosure (getParamBindings param <> env) body
  Let var e1 e2 -> do
    expr <- eval e1
    modifyTopM $ putValue var $ Val expr
    eval e2
  Apply e1 e2 -> do
    arg <- eval e2
    eval e1 >>= \case
      VClosure env body -> withFrame (EvalFrame arg env) $ eval body
      VBuiltin (Builtin _ func) -> func arg
  Dot expr name -> deref name <$> eval expr
  Record fields -> VRecord <$> mapM eval fields
  List exprs -> VList <$> mapM eval exprs
  If test ifTrue ifFalse -> eval test >>= \case
    VBool True -> eval ifTrue
    VBool False -> eval ifFalse
    val -> errorC ["Non-boolean test for if expression: ", render val]


-- | Converts a `ValOrArg` into a `Value`, by converting references to `Arg`
-- to the actual current argument value.
extractVal :: ValOrArg -> Eval Value
extractVal voa = case voa of
  Arg -> _fArgument <$> gets topFrame
  Val v -> return v
  VDot voa name -> deref name <$> extractVal voa

-- | Looks up a name (with failure possible) and extracts the value from it.
lookupNameOrError :: Name -> Eval Value
lookupNameOrError name = extractVal =<< findOrHardError name

-- | Calculates the closure (variables inherited from outside scope) of an
-- expression.
getClosure :: Expr -> Eval (HashMap Name ValOrArg)
getClosure = flip evalStateT [] . loop where
  loop :: Expr -> StateT [Set Name] Eval (HashMap Name ValOrArg)
  loop !(Variable name) = isBound name >>= \case
    True -> return mempty
    False -> do val <- lift $ lookupNameOrError name
                return $ H.singleton name (Val val)
  loop !(Lambda param body) = withNames (getNames param) $ loop body
  loop !(Let name e1 e2) = withNames (S.singleton name) $ loop2 e1 e2
  loop !(Apply e1 e2) = loop2 e1 e2
  loop !(Record fields) = foldl' (<>) mempty <$> mapM loop fields
  loop !(Typed e _) = loop e
  loop !(Dot e _) = loop e
  loop _ = return mempty
  loop2 a b = mappend <$> loop a <*> loop b
  withNames names action = modify (names :) *> action <* modify P.tail
  isBound name = get >>= look where
    look [] = return False
    look (names:rest) = if S.member name names then return True else look rest

-- | Gets the variable names out of an expression argument.
getNames :: Expr -> Set Name
getNames !(Variable name) = S.singleton name
getNames !(Record fields) = foldl' (<>) mempty $ fmap getNames fields
getNames _ = mempty

-- | Creates an evaluation environment from the patterns in an expression.
getParamBindings :: Expr -> HashMap Name ValOrArg
getParamBindings = go Arg where
  go val pattern = case pattern of
    Variable name -> H.singleton name val
    Record fields -> goFields val $ H.toList fields
    _ -> mempty
  goFields _ [] = mempty
  goFields val ((name, pattern):rest) = go (VDot val name) pattern
                                        <> goFields val rest

