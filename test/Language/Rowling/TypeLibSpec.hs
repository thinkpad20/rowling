{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Language.Rowling.TypeLibSpec (spec) where

import SpecHelper
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Language.Rowling.TypeLib

spec :: Spec
spec = do
  fromStringSpec
  normalizerSpec

fromStringSpec :: Spec
fromStringSpec = describe "fromstring" $ do
  it "should wrap captalized strings in TConst" $ do
    "Foo" `shouldBe` TConst "Foo"
  it "should wrap lower-case or $ strings in TVar" $ do
    "a" `shouldBe` TVar "a"
    "$t12" `shouldBe` TVar "$t12"
  it "should strip whitespace" $ do
    "  Foo  " `shouldBe` TConst "Foo"
    " bar" `shouldBe` TVar "bar"

normalizerSpec :: Spec
normalizerSpec = describe "type normalizer" $ do
  it "should replace single type variables" $ do
    normalize "$t8" `shouldBe` TVar "a"

  it "should replace type variables in functions" $ do
    normalize (TVar "$t1" ==> TVar "$t2") `shouldBe` (TVar "a" ==> TVar "b")

  it "should remember the names it made" $ do
    let recT = tRecord [("foo", TVar "$x"), ("bar", TVar "$x")]
    normalize recT `shouldBe` tRecord [("foo", TVar "a"), ("bar", TVar "a")]
    let funcT = TVar "$t1" ==> TVar "$t2" ==> TVar "$t1"
    normalize funcT `shouldBe` (TVar "a" ==> TVar "b" ==> TVar "a")

  it "should leave constants alone" $ do
    normalize "Bloop" `shouldBe` TConst "Bloop"
    normalize ("x" ==> "Bleep") `shouldBe` ("a" ==> TConst "Bleep")

  it "should normalize record types" $ do
    let t1 = tRecord [("foo", "$x")]
        t2 = TRecord [("bar", "$y")] (Just "$z")
    normalize t1 `shouldBe` tRecord [("foo", "a")]
    normalize t2 `shouldBe` TRecord [("bar", "a")] (Just "b")

  it "should update the state after making a replacement" $ do
    let t = TVar "x"
        (t', (name, mapping)) = runState (normalizeS t) ("a", mempty)
    t' `shouldBe` TVar "a"
    mapping `shouldBe` [("x", "a")]
    name `shouldBe` "b"

  it "should not have a problem if the variables start with 'a'" $ do
    normalize ("a" ==> "b" ==> "c") `shouldBe` ("a" ==> "b" ==> TVar "c")
    normalize ("c" ==> "a" ==> "b") `shouldBe` ("a" ==> "b" ==> TVar "c")

