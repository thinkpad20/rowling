{-# LANGUAGE NoImplicitPrelude #-}
module Language.Rowling.Evaluator where

import Prelude (String)
import Language.Rowling.Common
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Definitions.Values
import Language.Rowling.Evaluator.Evaluator
import Language.Rowling.Evaluator.Builtins
import Language.Rowling.Parser

-- * Running the evaluator monad.

runEval :: Eval a -> IO (a, EvalState)
runEval = runEvalWith $ def {_fEnvironment = Val <$> builtins}

runEvalWith :: EvalFrame -> Eval a -> IO (a, EvalState)
runEvalWith initFrame action = runStateT action' initState
  where
    action' = runReaderT action ()
    initState = def {_esStack = [initFrame]}

evalExpr :: Expr -> IO Value
evalExpr = evalWithBindings mempty

evalWithBindings :: HashMap Name Value -> Expr -> IO Value
evalWithBindings bindings expr = do
  let frame = def {_fEnvironment = Val <$> bindings <> builtins}
  (val, _) <- runEvalWith frame $ eval expr
  return val

evalIt :: String -> IO Value
evalIt input = case parseIt input of
  Left err -> error $ show err
  Right expr -> fst <$> runEval (eval expr)
