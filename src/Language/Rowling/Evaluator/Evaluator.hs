{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Rowling.Evaluator.Evaluator where

import qualified Prelude as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.ContextStack

import Language.Rowling.Common
import Language.Rowling.Definitions

-- | Evaluates an expression into a value.
eval :: Expr -> Eval Value
eval !expr = case expr of
  Int i -> return $ VInt i
  Float f -> return $ VFloat f
  String interp -> loop interp where
    loop (Plain s) = return $ VString s
    loop (Interp in1 e in2) = do
      VString s1 <- loop in1
      e' <- render <$> eval e
      VString s2 <- loop in2
      return $ VString $ s1 <> e' <> s2
  Constructor "True" -> return $ VBool True
  Constructor "False" -> return $ VBool False
  Constructor n -> return $ VTagged n []
  Variable var -> findOrHardError var
  Typed expr _ -> eval expr
  Lambda param body -> do
    env <- getClosure expr
    return $ VClosure env (Variable param) body
  Let var e1 e2 -> do
    modifyTopM . putValue var =<< eval e1
    eval e2
  Case e alts -> eval e >>= go alts where
    ps = fst <$> alts
    go [] v = errorC ["Pattern match failure: value ", render v,
                      " does not match any of the given patterns: ",
                      render ps]
    go ((pat, ex):rest) v = case patternMatch pat v of
      Nothing -> go rest v
      Just bs -> loadBindingsM bs >> eval ex
  Apply e1 e2 -> do
    arg <- eval e2
    eval e1 >>= \case
      -- See if the lambda's parameter matches the argument.
      VClosure env param body -> case patternMatch param arg of
        -- If it doesn't, it's an error!
        Nothing -> errorC ["Pattern match failure: argument ", render arg,
                           " does not match pattern ", render param]
        -- Otherwise, load the bindings and evaluate the body.
        Just bs -> withFrame (EvalFrame arg (bs <> env)) $ eval body
      VTagged n vs -> return $ VTagged n (vs ++ [arg])
      VBuiltin (Builtin _ func) -> func arg
  Dot expr name -> deref name <$> eval expr
  Record fields -> VRecord <$> mapM eval fields
  List exprs -> VArray <$> mapM eval exprs
  If test ifTrue ifFalse -> eval test >>= \case
    VBool True -> eval ifTrue
    VBool False -> eval ifFalse
    val -> errorC ["Non-boolean test for if expression: ", render val]

-- | Calculates the closure (variables inherited from outside scope) of an
-- expression. Only gets free variables, not bound. For example, in the
-- expression @λx -> f x 1@, the variable @x@ is bound, while the variable
-- @f@ is free. Therefore, @f@ will wind up in the closure, while @x@ won't.
getClosure :: Expr -> Eval (Record Value)
getClosure = flip evalStateT [] . loop where
  -- Uses a stack of sets of names to keep track of bound variables.
  loop :: Expr -> StateT [Set Name] Eval (Record Value)
  loop !(Variable name) = isBound name >>= \case
    True -> return mempty
    False -> do val <- lift $ findOrHardError name
                return [(name, val)]
  loop !(Lambda param body) = withName param $ loop body
  loop !(Let name e1 e2) = withName name $ loop2 e1 e2
  loop !(Apply e1 e2) = loop2 e1 e2
  loop !(Record fields) = foldl' (<>) mempty <$> mapM loop fields
  loop !(Typed e _) = loop e
  loop !(Dot e _) = loop e
  loop !(Case e alts) = map concat $ mapM go alts where
    go (p, e) = withNames (getNames p) (loop e)
  loop _ = return mempty
  loop2 a b = mappend <$> loop a <*> loop b
  withNames names action = modify (names :) *> action <* modify P.tail
  withName n = withNames (S.singleton n)
  isBound name = get >>= look where
    look [] = return False
    look (names:rest) = if S.member name names then return True else look rest

-- | Gets the variable names out of a pattern. These variables are bound in
-- downstream scopes.
getNames :: Pattern -> Set Name
getNames !(Variable name) = S.singleton name
getNames !(Record fields) = foldl' (<>) mempty $ fmap getNames fields
getNames !(Apply e1 e2)   = getNames e1 <> getNames e2
getNames _ = mempty
