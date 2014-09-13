{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Rowling.ParserSpec (main, spec) where

import SpecHelper
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parsing" $ do
  primitivesSpec
  dotSpec
  lambdasSpec
  letSpec
  recordsSpec
  applicationSpec
  ifSpec
  binarySpec
  listsSpec

primitivesSpec :: Spec
primitivesSpec = describe "primitives" $ do
  it "should parse primitives" $ do
    parseIt "3" `shouldBeR` Int 3
    parseIt "3.5" `shouldBeR` Float 3.5
    parseIt "\"hello\"" `shouldBeR` String "hello"
    parseIt "True" `shouldBeR` Constructor "True"
    parseIt "Just" `shouldBeR` Constructor "Just"

dotSpec :: Spec
dotSpec = describe "dots" $ do
  it "should parse dots" $ do
    parseIt "a.foo" `shouldBeR` Dot "a" "foo"

  it "should parse more complex stuff with dots" $ do
    parseIt "(x + y).z" `shouldBeR` Dot (binary "x" "+" "y") "z"

  it "should associate dots to the left" $ do
    parseIt "a.b.c" `shouldBeR` Dot (Dot "a" "b") "c"

lambdasSpec :: Spec
lambdasSpec = describe "lambdas" $ do
  it "should parse a simple lambda" $ do
    parseIt "&x -> x" `shouldBeR` Lambda "x" "x"

  it "should parse a nested lambda" $ do
    let output = Lambda "x" $ Lambda "y" $ Apply "y" "x"
    parseIt "&x -> &y -> y x" `shouldBeR` output

  it "should parse a lambda with a let statement" $ do
    let output = Lambda "x" $ Let "y" "x" "y"
    parseIt "&x -> let y = x; y" `shouldBeR` output

  it "should parse a nested lambda with a let statement" $ do
    let output = Lambda "x" $ Let "y" "x" (Lambda "z" $ Apply "z" "y")
    parseIt "&x -> let y = x; &z -> z y" `shouldBeR` output

  it "should parse a lambda with a pattern" $ do
    let output = Lambda "x" $ Case "x" [(Int 1, Int 2)]
    parseIt "&1 -> 2" `shouldBeR` output

letSpec :: Spec
letSpec = describe "let statements" $ do
  it "should parse a let statement" $ do
    parseIt "let foo = 1; foo" `shouldBeR` Let "foo" (Int 1) "foo"

  it "should parse a nested let statement" $ do
    let output = Let "x" "y" $ Let "z" "w" $ Int 3
    parseIt "let x = y; let z = w; 3" `shouldBeR` output

  it "should parse function declarations" $ do
    let output = Let "f" (Lambda "x" (binary "x" "+" (Int 3))) "f"
    parseIt "let f x = x + 3; f" `shouldBeR` output

  it "should parse function declarations with multiple args" $ do
    let output = Let "f" (Lambda "x" (Lambda "y" (binary "x" "+" "y"))) "f"
    parseIt "let f x y = x + y; f" `shouldBeR` output

  it "should parse function declarations with patterns" $ do
    let output = Let "f" (Lambda "a" (Case "a" [(Int 1, Int 2)])) "f"
    parseIt "let f 1 = 2; f" `shouldBeR` output

  it "should parse function declarations with multiple patterns" $ do
    let body = Case "a" [(Int 1, Int 2), ("y", binary "y" "+" (Int 3))]
    let output = Let "f" (Lambda "a" body) "f"
    parseIt "let f 1 = 2 | f y = y + 3; f" `shouldBeR` output

  it "should parse function declarations with multiple args/patterns" $ do
    let input = "let f 1 2 = 0 | f x y = x + y; f"
    -- This desugars to:
    -- let f a b = if [a, b] is [1, 2] -> 0 | [x, y] -> [x + y]; f
        body = Case ["a", "b"] [([Int 1, Int 2], Int 0),
                                (["x", "y"], binary "x" "+" "y")]
        output = Let "f" (Lambda "a" $ Lambda "b" body) "f"
    parseIt input `shouldBeR` output

  it "should parse symbol function declarations" $ do
    let input = "let x +-+ y = x * (y + x); 0"
        output = Let "+-+" (Lambda "x" $ Lambda "y" $
                             binary "x" "*" (binary "y" "+" "x")) (Int 0)
    parseIt input `shouldBeR` output

  it "should parse symbol function declarations with multiple patterns" $ do
    let input = "let 1 +-+ 0 = 1 | x +-+ y = x * y; 0"
        body = Case ["a", "b"] [([Int 1, Int 0], Int 1),
                                (["x", "y"], binary "x" "*" "y")]
        output = Let "+-+" (Lambda "a" $ Lambda "b" body) (Int 0)
    parseIt input `shouldBeR` output

  it "should fail if the wrong function name is used" $ do
    let input = "let foo 0 = 1 | bar 1 = 2; baz"
    parseIt input `shouldHaveErr` "Expected function named \"foo\""

  it "should fail if the wrong number of arguments is used" $ do
    let input = "let foo 0 = 1 | foo 1 2 = 3; baz"
    parseIt input `shouldHaveErr` "Wrong number of arguments, expected 1"

  it "should fail if adding patterns to function with all variables" $ do
    let input = "let foo x = 1 | foo y = 2; foo"
    parseIt input `shouldHaveErr` "unexpected"
    let input = "let foo = 1 | foo = 2; foo"
    parseIt input `shouldHaveErr` "unexpected"

recordsSpec :: Spec
recordsSpec = describe "records" $ do
  it "should parse records" $ do
    let record = Record [("foo", Int 2), ("bar", Float 3)]
    parseIt "(foo=2, bar=3.0)" `shouldBeR` record

  it "should parse tuples" $ do
    let tup = Record [("0", "foo"), ("1", "False")]
    parseIt "(foo, False)" `shouldBeR` tup

  it "should parse tuples mixed with records" $ do
    let rec = Record [("0", "a"), ("1", "b"), ("foo", "c")]
    parseIt "(a, b, foo=c)" `shouldBeR` rec

applicationSpec :: Spec
applicationSpec = describe "applications" $ do
  it "should parse an application with variables" $ do
    parseIt "x y" `shouldBeR` Apply "x" "y"

  it "should parse an application with constants" $ do
    parseIt "f True" `shouldBeR` Apply "f" "True"
    parseIt "x 1" `shouldBeR` Apply "x" (Int 1)

  it "should nest applications to the left" $ do
    parseIt "a b c" `shouldBeR` Apply (Apply "a" "b") "c"

  it "should be able to apply things in parentheses" $ do
    parseIt "(a b) c" `shouldBeR` Apply (Apply "a" "b") "c"
    parseIt "a (b c)" `shouldBeR` Apply "a" (Apply "b" "c")

  it "should be able to apply constructors" $ do
    parseIt "Just 7" `shouldBeR` Apply "Just" (Int 7)

ifSpec :: Spec
ifSpec = describe "if statements" $ do
  it "should parse if statements" $ do
    let output = If "False" (Int 1) (Int 2)
    parseIt "if False then 1 else 2" `shouldBeR` output

  it "should parse nested if statements" $ do
    let output = If "True" (If "False" "a" "b") "c"
    parseIt "if True then if False then a else b else c" `shouldBeR` output
    let output = If "True" "a" (If "False" "b" "c")
    parseIt "if True then a else if False then b else c" `shouldBeR` output
    let output = If (If "True" "a" "b") "c" "d"
    parseIt "if if True then a else b then c else d" `shouldBeR` output

  it "should parse cases statements" $ do
    let output = Case "x" [(Int 1, Float 2), (Int 2, Float 0)]
    parseIt "if x is 1 -> 2.0 | 2 -> 0.0" `shouldBeR` output

    let input = "if x * 3 is 5 -> 0 | Just 7 -> 1 | foo -> foo"
        output = Case (binary "x" "*" (Int 3)) [(Int 5, Int 0),
                                                (Apply "Just" (Int 7), Int 1),
                                                ("foo", "foo")]
    parseIt input `shouldBeR` output

binarySpec :: Spec
binarySpec = describe "binary operations" $ do
  let bin a b = binary a "+" b
  it "should do addition" $ do
    parseIt "a + b" `shouldBeR` bin "a" "b"
  it "should do nested addition" $ do
    parseIt "a + b + c" `shouldBeR` bin (bin "a" "b") "c"
  let bin a b = binary a "-" b
  it "should do subtraction" $ do
    parseIt "a - b" `shouldBeR` bin "a" "b"
  it "should do nested subtraction" $ do
    parseIt "a - b - c" `shouldBeR` bin (bin "a" "b") "c"
  it "should respect parentheses" $ do
    parseIt "a - (b - c)" `shouldBeR` bin "a" (bin "b" "c")

  it "should do weird custom binary operators" $ do
    parseIt "a +-+ b" `shouldBeR` binary "a" "+-+" "b"

  it "should not take key symbols as binary operators" $ do
    parseIt "a = b" `shouldHaveErr` ""

listsSpec :: Spec
listsSpec = describe "list literals" $ do
  it "should do list literals" $ do
    parseIt "[a, b, c]" `shouldBeR` ["a", "b", "c"]

  it "should do nested list literals" $ do
    parseIt "[a, [b, c], d]" `shouldBeR` ["a", ["b", "c"], "d"]
