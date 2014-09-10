{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Language.Rowling.TypeCheckerSpec (spec) where

import SpecHelper
import ClassyPrelude hiding (assert)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Language.Rowling.Definitions.Expressions
import Language.Rowling.Definitions.Types
import Language.Rowling.TypeCheck.TypeChecker

spec :: Spec
spec = do
  describe "primitive types" $ do
    it "should type literals" $ do
      typeExpr (Int 0) `shouldBeR` "Int"
      typeExpr (Float 0) `shouldBeR` "Float"
      typeExpr (String "hey there") `shouldBeR` "String"
      typeExpr "True" `shouldBeR` "Bool"

  describe "complex types" $ do
    it "should type lists" $ do
      typeExpr [Int 0, Int 1] `shouldBeR` TApply "List" "Int"

    it "should type functions" $ do
      typeExprN (Lambda "x" "x") `shouldBeR` "a" ==> "a"
      typeExprN (Lambda "x" $ Lambda "y" $ Apply "y" "x")
        `shouldBeR`
        "a" ==> ("a" ==> "b") ==> "b"

    it "should type applications" $ do
      typeExpr (Apply (Lambda "x" "x") (Int 1)) `shouldBeR` "Int"
      typeExpr (Apply (Lambda "x" "x") [Int 1]) `shouldBeR` TApply "List" "Int"
      typeExpr (Apply (Lambda "x" ["x"]) (Float 1))
        `shouldBeR`
        TApply "List" "Float"

    it "should type let statements" $ do
      typeExpr (Let "foo" (Int 1) "foo") `shouldBeR` "Int"
      typeExprN (Lambda "x" $ Let "y" (Int 1) "y") `shouldBeR` "a" ==> "Int"
