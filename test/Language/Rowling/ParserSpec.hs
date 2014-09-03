module Language.Rowling.ParserSpec (main, spec) where

import SpecHelper
import Language.Rowling.Parser

main :: IO ()
main = hspec $ spec >> spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` True
