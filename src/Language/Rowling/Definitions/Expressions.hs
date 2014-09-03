{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Rowling.Definitions.Expressions where

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import qualified GHC.Exts as GHC

import Language.Rowling.Common
import Language.Rowling.Definitions.Types

-- | The expression type.
data Expr = Int Integer -- ^ An integer literal.
          | Float Double -- ^ An floating-point literal.
          | String Text -- ^ A string literal.
          | Bool Bool -- ^ A boolean literal.
          | Variable Name -- ^ A variable.
          | Typed Expr Type -- ^ An expression with annotated type.
          | Lambda Pattern Expr -- ^ A lambda expression.
          | Let Name Expr Expr -- ^ A let expression.
          | Apply Expr Expr -- ^ An application.
          | Dot Expr Name -- ^ A field dereference.
          | List (Vector Expr) -- ^ A list literal.
          | Record (HashMap Name Expr) -- ^ A record literal.
          | If Expr Expr Expr -- ^ A conditional expression.
          deriving (Show, Eq)

-- | Patterns are just expressions, although they're used differently.
type Pattern = Expr

instance IsString Expr where
  fromString = Variable . fromString

instance IsList Expr where
  type Item Expr = Expr
  fromList = List . GHC.fromList
  toList (List es) = GHC.toList es
  toList _ = error "Not a list expression"

instance Render Expr where
  render = \case
    Int i -> render i
    Float f -> render f
    String t -> render t
    Bool b -> render b
    Variable name -> name
    Typed expr typ -> render expr <> " :: " <> render typ
    Lambda e1 e2 -> "λ" <> renderParens e1 <> " -> " <> render e2
    Let name e1 e2 -> "let " <> name <> " = " <> render e1 <> "; " <> render e2
    Apply (Apply (Variable name) e1) e2 | isOp name ->
      renderParens e1 <> " " <> name <> " " <> renderParens e2
    Apply (e1@(Apply _ _)) e2 -> render e1 <> " " <> renderParens e2
    Apply e1 e2 -> renderParens e1 <> " " <> renderParens e2
    Dot e name -> renderParens e <> "." <> name
    Record fields -> "(" <> T.intercalate ", " (join fields) <> ")" where
      join = map joinNE . H.toList
      joinNE (name, expr) = name <> ": " <> render expr
    List es -> "[" <> T.intercalate ", " renderedList <> "]" where
      renderedList = toList $ map render es
  renderParens e = case e of
    Apply _ _ -> parens
    Typed _ _ -> parens
    Lambda _ _ -> parens
    _ -> render e
    where parens = "(" <> render e <> ")"

-- | Characters legal in symbols.
symChars :: P.String
symChars = "+-/*:|&^%$><"

-- | A binary expression is syntactic sugar for a nested application.
binary :: Expr -> Name -> Expr -> Expr
binary e1 op e2 = Apply (Apply (Variable op) e1) e2

-- | Tests if the string is an operator (symbol).
isOp :: Text -> Bool
isOp = T.all (`S.member` (S.fromList symChars))