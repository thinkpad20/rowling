{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.Rowling.TypeCheck.Builtins (
  builtInTypes
  ) where

import Language.Rowling.Common
import Language.Rowling.Definitions.Types

builtInTypes :: TypeMap
builtInTypes = [("+", "Int" ==> "Int" ==> "Int")]
