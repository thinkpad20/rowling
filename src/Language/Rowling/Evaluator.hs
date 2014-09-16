{-# LANGUAGE NoImplicitPrelude #-}
module Language.Rowling.Evaluator where

import Prelude (String)
import Language.Rowling.Common
import Language.Rowling.Definitions
import Language.Rowling.Evaluator.Evaluator
import Language.Rowling.Evaluator.Builtins
import Language.Rowling.Parser

-- * Running the evaluator monad.

-- ** Evaluating with returned state

-- | Runs an evaluator with only the builtin bindings.
runEval :: Eval a -> IO (a, EvalState)
runEval = runEvalWith [def {_fEnvironment = builtins}]

-- | Runs an evaluator with the given frames as initial state.
runEvalWith :: [EvalFrame] -> Eval a -> IO (a, EvalState)
runEvalWith initFrames action = runStateT action' initState
  where
    action' = runReaderT action ()
    initState = def {_esStack = initFrames}

-- ** Evaluating strings

-- | Evaluates a raw expression string into a value. Throws IO errors if
-- the string doesn't parse, if the expression is ill-typed, or if there's a
-- runtime failure during evaluation.
evalIt :: String -> IO Value
evalIt input = case parseIt input of
  Left err -> error $ show err
  Right expr -> fst <$> runEval (eval expr)

-- ** Evalutating ASTs

-- | Evaluates an expression with only the builtin bindings.
evalExpr :: Expr -> IO Value
evalExpr = evalWithBindings mempty

-- | Evaluates an expression with the given bindings and builtins.
evalWithBindings :: Record Value -> Expr -> IO Value
evalWithBindings bindings expr = do
  let baseFrame = def {_fEnvironment = builtins}
      startFrame = def {_fEnvironment = bindings}
  fst <$> runEvalWith [startFrame, baseFrame] (eval expr)
