{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Rowling.EvaluatorSpec (main, spec) where

import SpecHelper
import ClassyPrelude
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Definitions.Values
import Language.Rowling.Evaluator

main :: IO ()
main = hspec spec


-- Some useful functions for tests
idfunc, singlelist, capturingFunc, apply, weird :: Expr
idfunc = Lambda "x" "x"
singlelist = Lambda "x" ["x"]
capturingFunc = Let "x" (Int 3) (Lambda "y" (Apply "y" "x"))
apply = Lambda "x" $ Lambda "y" $ Apply "x" "y"
weird = Lambda "x" $ Let "y" "x" $ Lambda "z" $ Apply "z" "y"

spec :: Spec
spec = describe "evaluation" $ do
  primitivesSpec

  describe "records" $ do
    it "should evaluate records" $ do
      let input = Record [("foo", Int 2), ("bar", Float 3)]
          output = VRecord [("foo", VInt 2), ("bar", VFloat 3)]
      evalExpr input `shouldBeM` output

  describe "if statements" $ do
    it "should evaluate if statements" $ do
      let input = If "False" (Int 1) (Int 2)
          output = VInt 2
      evalExpr input `shouldBeM` output

  lambdaSpec
  applicationSpec
  builtinSpec
  listsSpec
  caseSpec

  exampleEvaluations

primitivesSpec :: Spec
primitivesSpec = describe "primitives" $ do
  it "should evaluate primitives" $ do
    evalExpr (Int 3) `shouldBeM` VInt 3
    evalExpr (Float 3) `shouldBeM` VFloat 3
    evalExpr (String "hello") `shouldBeM` VString "hello"
    evalExpr "True" `shouldBeM` VBool True

lambdaSpec :: Spec
lambdaSpec = describe "lambdas" $ do
  it "should evaluate a lambda with no captures" $ do
    let input = idfunc
        output = VClosure {
          _cEnvironment = [],
          _cPattern = "x",
          _cBody = "x"
        }
    evalExpr input `shouldBeM` output
    let input = singlelist
        output = VClosure {
          _cEnvironment = [],
          _cPattern = "x",
          _cBody = ["x"]
        }
    evalExpr input `shouldBeM` output

  it "should evaluate a lambda with captures" $ do
    let input = capturingFunc
        output = VClosure {
          _cEnvironment = [("x", VInt 3)],
          _cPattern = "y",
          _cBody = Apply "y" "x"
        }
    evalExpr input `shouldBeM` output

  it "should evaluate nested lambdas" $ do
    let input1 = apply
        output1 = VClosure {
          _cEnvironment = [],
          _cPattern = "x",
          _cBody = Lambda "y" $ Apply "x" "y"
        }
    evalExpr input1 `shouldBeM` output1

    let input2 = weird
        output2 = VClosure {
          _cEnvironment = [],
          _cPattern = "x",
          _cBody = Let "y" "x" $ Lambda "z" $ Apply "z" "y"
        }
    evalExpr input2 `shouldBeM` output2

applicationSpec :: Spec
applicationSpec = describe "function application" $ do
  it "should evaluate an application" $ do
    let input = Apply idfunc $ Int 0
        output = VInt 0
    evalExpr input `shouldBeM` output

  it "should apply a nested lambda" $ do
    -- (λx -> λy -> x y) (λx -> x)
    let input = Apply apply idfunc
        output = VClosure {
          _cEnvironment = [
            ("x", VClosure {
              _cEnvironment = [],
              _cPattern = "x",
              _cBody = "x"
            })
          ],
          _cPattern = "y",
          _cBody = Apply "x" "y"
        }
    evalExpr input `shouldBeM` output

  it "should apply that function and act like the id function" $ do
    let input = Apply (Apply apply idfunc) $ String "wazzap"
        output = VString "wazzap"
    evalExpr input `shouldBeM` output

  it "should build tagged unions" $ do
    let input = Apply (Apply "Pair" (Int 1)) (Int 2)
        output = VTagged "Pair" [VInt 1, VInt 2]
    evalExpr input `shouldBeM` output

    let input = Apply (Apply "Pair" (Apply "Just" (Int 1))) (Int 2)
        output = VTagged "Pair" [VTagged "Just" [VInt 1], VInt 2]
    evalExpr input `shouldBeM` output

  describe "records in functions" $ do
    it "should evaluate record arguments" $ do
      let func = Lambda "r" $ "r" `Dot` "foo"
          input = Apply func $ Record [("foo", Int 10)]
          output = VInt 10
      evalExpr input `shouldBeM` output

    it "should return records" $ do
      let func = Lambda "x" $ Record [("foo", "x"), ("bar", "x")]
          input1 = Int 10
          output1 = VRecord [("foo", VInt 10), ("bar", VInt 10)]
          input2 = Record [("baz", Int 10)]
          output2 = VRecord [("foo", VRecord [("baz", VInt 10)]),
                             ("bar", VRecord [("baz", VInt 10)])]
      evalExpr (Apply func input1) `shouldBeM` output1
      evalExpr (Apply func input2) `shouldBeM` output2

      let func = Lambda "x" $ Record [("a", "x" `Dot` "b"),
                                      ("b", "x" `Dot` "a")]
          input = Record [("a", Int 1), ("b", Float 1)]
          output = VRecord [("b", VInt 1), ("a", VFloat 1)]
      evalExpr (Apply func input) `shouldBeM` output

    it "should evaluate records that contain functions" $ do
      let func = Lambda "r" $ Apply ("r" `Dot` "f") (Int 6)
          input = Apply func $ Record [("f", idfunc)]
          output = VInt 6
      evalExpr input `shouldBeM` output

  describe "lists in functions" $ do
    it "should produce a list" $ do
      let input = (Apply (Lambda "x" ["x"]) (Float 1))
          output = [VFloat 1]
      evalExpr input `shouldBeM` output

      let input = (Apply (Lambda "x" ["x", "x"]) (Float 1))
          output = [VFloat 1, VFloat 1]
      evalExpr input `shouldBeM` output

      let input = (Apply (Lambda "x" [Float 2, "x"]) (Float 1))
          output = [VFloat 2, VFloat 1]
      evalExpr input `shouldBeM` output

  describe "pattern matching lambdas" $ do
    it "should destructure its argument" $ do
      -- (λ(Foo x) -> x + 1) (Foo 2)
      let func = Lambda (Apply "Foo" "x") (binary "x" "+" (Int 1))
          arg = Apply "Foo" (Int 2)
          input = Apply func arg
          output = VInt 3
      evalExpr input `shouldBeM` output

    it "should destructure nested arguments" $ do
      -- (λ(Foo x (Bar y)) -> x + y) (Foo 2 (Bar 1))
      let param = Apply (Apply "Foo" "x") (Apply "Bar" "y")
          func = Lambda param (binary "x" "+" "y")
          arg = Apply (Apply "Foo" (Int 2)) (Apply "Bar" (Int 1))
          input = Apply func arg
          output = VInt 3
      evalExpr input `shouldBeM` output

      -- (λ(Foo (Bar x) y) -> x + y) (Foo (Bar 1) 3)
      let param = Apply (Apply "Foo" (Apply "Bar" "x")) "y"
          func = Lambda param (binary "x" "+" "y")
          arg = Apply (Apply "Foo" (Apply "Bar" (Int 1))) (Int 3)
          input = Apply func arg
          output = VInt 4
      evalExpr input `shouldBeM` output

caseSpec :: Spec
caseSpec = describe "case statements" $ do
  it "should match appropriately" $ do
    let input e = Case e [("x", "x")]
    let exprs :: [Expr] -- type sig for type inference
        exprs = [Int 1, Float 2, String "hey", binary (Int 1) "+" (Int 2)]
    forM_ exprs $ \e -> do
      v <- evalExpr e
      evalExpr (input e) `shouldBeM` v

  it "should destructure" $ do
    let case_ e = Case e [(Apply "Just" "x", binary "x" "+" (Int 1)),
                          ("Nothing", Int 0)]
    evalExpr (case_ "Nothing") `shouldBeM` VInt 0
    evalExpr (case_ (Apply "Just" (Int 2))) `shouldBeM` VInt 3
  it "should error if nothing matches" $
    pendingWith "Don't know how to assert an error"

builtinSpec :: Spec
builtinSpec = describe "builtins" $ do
  let test l op r result =
        evalExpr (binary l op r) `shouldBeM` result

  describe "addition" $ do
    it "should do ints" $ do
      test (Int 10) "+" (Int 2) (VInt 12)

    it "should do floats" $ do
      test (Float 7) "+" (Float 3) (VFloat 10)

    it "should do floats with ints and vice versa" $ do
      test (Float 1) "+" (Float 2) (VFloat 3)
      test (Float 1.5) "+" (Int 2) (VFloat 3.5)
      test (Int 1) "+" (Float 2) (VFloat 3)

  describe "multiplication" $ do
    it "should do ints" $ do
      test (Int 1) "*" (Int 2) (VInt 2)
      test (Int (-10)) "*" (Int 2) (VInt (-20))

    it "should do floats" $ do
      test (Float 1) "*" (Float 2) (VFloat 2)
      test (Float 1) "*" (Int 2) (VFloat 2)

    it "should do floats with ints and vice versa" $ do
      test (Float 1) "*" (Float 2) (VFloat 2)
      test (Float 1.5) "*" (Int 2) (VFloat 3)
      test (Int 1) "*" (Float 2) (VFloat 2)

  describe "division" $ do
    it "should do ints" $ do
      test (Int 10) "/" (Int 2) (VInt 5)

    it "should round ints down when not an int result" $ do
      test (Int 1) "/" (Int 2) (VInt 0)
      test (Int 7) "/" (Int 3) (VInt 2)

    it "should do floats" $ do
      test (Float 7) "/" (Float 3) (VFloat (7/3))

    it "should do floats with ints and vice versa" $ do
      test (Float 1) "/" (Float 2) (VFloat 0.5)
      test (Float 1) "/" (Int 2) (VFloat 0.5)
      test (Int 1) "/" (Float 2) (VFloat 0.5)

  describe "comparison" $ do
    it "should do ints" $ do
      test (Int 10) ">" (Int 2) (VBool True)
      test (Int 10) ">=" (Int 2) (VBool True)
      test (Int 10) "<" (Int 2) (VBool False)
      test (Int 10) "<=" (Int 2) (VBool False)
      test (Int 10) "==" (Int 2) (VBool False)
      test (Int 10) "==" (Int 10) (VBool True)
      test (Int 10) "<=" (Int 10) (VBool True)
      test (Int 10) ">=" (Int 10) (VBool True)
      test (Int 10) "!=" (Int 10) (VBool False)

    it "should do floats" $ do
      test (Float 10) ">" (Float 2) (VBool True)
      test (Float 10) ">=" (Float 2) (VBool True)
      test (Float 10) "<" (Float 2) (VBool False)
      test (Float 10) "<=" (Float 2) (VBool False)
      test (Float 10) "==" (Float 2) (VBool False)
      test (Float 10) "==" (Float 10) (VBool True)
      test (Float 10) "<=" (Float 10) (VBool True)
      test (Float 10) ">=" (Float 10) (VBool True)
      test (Float 10) "!=" (Float 10) (VBool False)

    it "should do floats with ints and vice versa" $ do
      test (Int 10) ">" (Float 2) (VBool True)
      test (Float 10) ">=" (Int 2) (VBool True)
      test (Int 10) "<" (Float 2) (VBool False)
      test (Float 10) "<=" (Int 2) (VBool False)
      test (Int 10) "==" (Float 2) (VBool False)

  describe "boolean operations" $ do
    it "should negate a bool" $ do
      evalExpr (Apply "not" "True") `shouldBeM` VBool False
      evalExpr (Apply "not" "False") `shouldBeM` VBool True

    it "should AND bools" $ do
      evalExpr (binary "True" "&&" "False") `shouldBeM` VBool False
      evalExpr (binary "True" "&&" "True") `shouldBeM` VBool True
      evalExpr (binary "False" "&&" "True") `shouldBeM` VBool False
      evalExpr (binary "False" "&&" "False") `shouldBeM` VBool False

    it "should OR bools" $ do
      evalExpr (binary "True" "||" "False") `shouldBeM` VBool True
      evalExpr (binary "True" "||" "True") `shouldBeM` VBool True
      evalExpr (binary "False" "||" "True") `shouldBeM` VBool True
      evalExpr (binary "False" "||" "False") `shouldBeM` VBool False

  describe "map" $ do
    it "should apply a function to each element of a list" $ do
      let list = List ["True", "True", "False"]
          input = Apply (Apply "each" list) "not"
          output = VArray [VBool False, VBool False, VBool True]
      evalExpr input `shouldBeM` output

listsSpec :: Spec
listsSpec = describe "lists" $ do
  it "should evaluate lists" $ do
    let nums = [1..10]
    evalExpr (List $ map Int nums) `shouldBeM` VArray $ map VInt nums
    evalExpr (List $ map Float nums) `shouldBeM` VArray $ map VFloat nums
    let strs = ["hello", "hey", "hi"]
    evalExpr (List $ map String strs) `shouldBeM`
      VArray $ map VString strs

exampleEvaluations :: Spec
exampleEvaluations = describe "example evaluations" $ do
  describe "factorial" $ do
    it "should compute a factorial" $ do
      let fact = Lambda "n" $ If (binary "n" "<" (Int 1))
                                 (Int 1)
                                 (binary "n" "*" $
                                   Apply "fact" $ binary "n" "-" (Int 1))
          factOf n = Let "fact" fact $ Apply "fact" n
      evalExpr (factOf (Int 5)) `shouldBeM` VInt 120
      evalExpr (factOf (Float 5)) `shouldBeM` VFloat 120
      evalExpr (factOf (Int 10)) `shouldBeM` VInt 3628800
