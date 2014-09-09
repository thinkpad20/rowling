{-# LANGUAGE NoImplicitPrelude #-}
module SpecHelper
    ( module Test.Hspec
    , module Language.Rowling.Common
    , shouldBeM, shouldBeR, shouldHaveErr, shouldBeMR
    ) where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Language.Rowling.Common

-- | Runs `shouldBe` on the result of an IO action.
shouldBeM :: (Show a, Eq a) => IO a -> a -> IO ()
shouldBeM action expected = do
  result <- action
  result `shouldBe` expected

infixr 0 `shouldBeM`

-- | Asserts that the first argument is a `Right` value equal to the second
-- argument.
shouldBeR :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
shouldBeR x y = do
  shouldSatisfy x isRight
  let Right x' = x
  x' `shouldBe` y

infixr 0 `shouldBeR`

-- | Asserts that the argument is a `Left` value, containing something which
-- when `Show`n contains the provided substring.
shouldHaveErr :: (Show a, Show b)
              => Either a b -- ^ Should be a `Left` value.
              -> String     -- ^ Error should contain this.
              -> IO ()
shouldHaveErr x msg = do
  shouldSatisfy x isLeft
  let Left err = x
  show err `shouldSatisfy` isInfixOf msg

-- | Runs `shouldBeR` on the result of an IO action.
shouldBeMR :: (Show a, Show x, Eq a, Eq x) => IO (Either x a) -> a -> IO ()
shouldBeMR action expected = action >>= flip shouldBeR expected
