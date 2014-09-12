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
