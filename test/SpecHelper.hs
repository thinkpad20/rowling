module SpecHelper
    ( module Test.Hspec
    , module Authoring.Common
    , shouldBeM, shouldBeR, shouldBeMR
    ) where

import Test.Hspec

shouldBeM :: (Show a, Eq a) => IO a -> a -> IO ()
shouldBeM action expected = do
  result <- action
  result `shouldBe` expected

infixr 0 `shouldBeM`

shouldBeR :: (Show a, Show b, Eq a, Eq b) => Either a b -> b -> IO ()
shouldBeR x y = x `shouldBe` Right y

infixr 0 `shouldBeR`

shouldBeMR :: (Show a, Show x, Eq a, Eq x) => IO (Either x a) -> a -> IO ()
shouldBeMR action expected = action `shouldBeM` Right expected
