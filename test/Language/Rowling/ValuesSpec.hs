{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Rowling.ValuesSpec (main, spec) where

import SpecHelper
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Definitions.Values

main :: IO ()
main = hspec $ spec >> spec

spec :: Spec
spec = patternMatchSpec

patternMatchSpec :: Spec
patternMatchSpec = describe "pattern matching" $ do
  it "should match literals that equal" $ do
    match (Int 1) (VInt 1)
    match (Float 1) (VFloat 1)
    match "False" (VBool False)
    match (String "wazzap") (VString "wazzap")

  it "should not match literals that are not equal" $ do
    noMatch (Int 1) (VInt 2)
    noMatch (Float 1) (VFloat 2)
    noMatch (String "hey") (VString "yo")

  it "should match variables" $ do
    let vals :: [Value] = [VInt 1, VFloat 1, VBool True, ["hello", "world"]]
    forM_ vals $ \val ->
      matchWith (Variable "x") val [("x", val)]

  it "should match list patterns" $ do
    match [Int 1, Int 2] [VInt 1, VInt 2]
    matchWith ["a", Int 1] [VInt 0, VInt 1] [("a", VInt 0)]
    noMatch ["a", Int 1] [VInt 0, VInt 2]

  describe "record patterns" $ do
    it "should match when keys match" $ do
      match (Record [("x", Int 1), ("y", Int 3)])
            (VRecord [("x", VInt 1), ("y", VInt 3)])
      matchWith (Record [("x", "a"), ("y", "b")])
                (VRecord [("x", VInt 2), ("y", VFloat 5)])
                [("a", VInt 2), ("b", VFloat 5)]

    it "should match when there are extra keys in the value" $ do
      match (Record [("x", Int 1), ("y", Int 3)])
            (VRecord [("x", VInt 1), ("y", VInt 3), ("z", VInt 0)])
      matchWith (Record [("x", "a"), ("y", "b")])
                (VRecord [("x", VInt 2), ("y", VFloat 5), ("z", VInt 0)])
                [("a", VInt 2), ("b", VFloat 5)]

    it "should NOT match when there are extra keys in the pattern" $ do
      noMatch (Record [("x", Int 1), ("y", Int 3)])
              (VRecord [("x", VInt 2), ("y", VInt 3)])
      noMatch (Record [("x", Int 1), ("y", Int 3)])
              (VRecord [("x", VInt 2)])

  describe "haskell builtins" $ do
    it "should use haskell booleans" $ do
      match "True" (VBool True)
      match "False" (VBool False)
      noMatch "False" (VBool True)
      noMatch "True" (VBool False)

    it "should use haskell maybes" $ do
      match "None" (VMaybe Nothing)
      match (Apply "Some" (Int 3)) (VMaybe (Just $ VInt 3))
      noMatch "None" (VMaybe $ Just (VInt 0))

  describe "compound expressions" $ do
    it "should match singleton constructors" $ do
      match "Foo" (VTagged "Foo" [])
    it "should match applied constructors" $ do
      match (Apply "Foo" (Int 1)) (VTagged "Foo" [VInt 1])
    it "should assign variables correctly" $ do
      matchWith (Apply "Foo" "x") (VTagged "Foo" [VInt 1]) [("x", VInt 1)]
    it "should handle multiple variables" $ do
      matchWith (Apply (Apply "A" "b") "c") (VTagged "A" [VInt 1, VFloat 2])
        [("b", VInt 1), ("c", VFloat 2)]
    it "should handle a mix of variables and constants" $ do
      matchWith (Apply (Apply "A" "b") (Int 1)) (VTagged "A" [VInt 1, VInt 1])
        [("b", VInt 1)]
    it "should reject incompatible matches" $ do
      noMatch (Apply (Apply "A" "b") (Int 1)) (VTagged "A" [VInt 1, VInt 0])
    it "should reject when the length isn't correct" $ do
      noMatch (Apply (Apply "A" "b") (Int 1)) (VTagged "A" [VInt 0])
    it "should handle nested compound expressions" $ do
      -- Pattern: @A (B 1) x@. Value: @A (B 1) "hello"@
      matchWith (Apply (Apply "A" (Apply "B" (Int 1))) "x")
                (VTagged "A" [VTagged "B" [VInt 1], VString "hello"])
                [("x", VString "hello")]
      -- Pattern: @A (B 1) x@. Value: @A (B 2) "hello"@
      noMatch (Apply (Apply "A" (Apply "B" (Int 1))) "x")
              (VTagged "A" [VTagged "B" [VInt 2], VString "hello"])

  where
    matchWith :: Pattern -> Value -> HashMap Name Value -> IO ()
    matchWith p v bs = patternMatch p v `shouldBeJ` bs
    match :: Pattern -> Value -> IO ()
    match p v = matchWith p v []
    noMatch :: Pattern -> Value -> IO ()
    noMatch p v = shouldBeN $ patternMatch p v

