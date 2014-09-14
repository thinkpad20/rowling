{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.Rowling.TypeCheck.Builtins (
  builtInTypes
  ) where

import Language.Rowling.Common
import Language.Rowling.Definitions.Types
import qualified Data.Set as S

builtInTypes :: TypeMap
builtInTypes = [("+", "Int" ==> "Int" ==> "Int"),
                ("Some", poly ["a"] $ "a" ==> TApply "Maybe" "a"),
                ("None", poly ["a"] $ TApply "Maybe" "a")]
             where poly names t = Polytype (S.fromList names) t
