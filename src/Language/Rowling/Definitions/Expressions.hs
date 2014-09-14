{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Rowling.Definitions.Expressions where

import qualified Prelude as P
import Data.Char (isUpper)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H
import qualified GHC.Exts as GHC

import Language.Rowling.Common
import Language.Rowling.Definitions.Types

-- | The expression type.
data Expr = Int Integer -- ^ An integer literal.
          | Float Double -- ^ An floating-point literal.
          | String Interp -- ^ A string literal with interpolated expressions.
          | Variable Name -- ^ A variable.
          | Constructor Name -- ^ A constructor (e.g. @Just@, @False@, etc).
          | Typed Expr Type -- ^ An expression with annotated type.
          | Lambda Name Expr -- ^ A lambda expression.
          | Case Expr [(Pattern, Expr)] -- ^ A case statement.
          | Let Name Expr Expr -- ^ A let expression.
          | Apply Expr Expr -- ^ An application.
          | Dot Expr Name -- ^ A field dereference.
          | List (Vector Expr) -- ^ A list literal.
          | Record (Record Expr) -- ^ A record literal.
          | If Expr Expr Expr -- ^ A conditional expression.
          deriving (Show, Eq)

-- | Patterns are just expressions, although they're used differently.
type Pattern = Expr

-- | A string with interpolated expressions.
data Interp = Plain Text
            | Interp Interp Expr Interp
            deriving (Show, Eq)

instance IsString Expr where
  fromString "" = error "Empty variable string"
  fromString var@(c:_) | isUpper c = Constructor $ fromString var
                       | otherwise = Variable $ fromString var

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
    Constructor name -> name
    Variable name -> name
    Typed expr typ -> render expr <> " :: " <> render typ
    Lambda name e2 -> "λ" <> name <> " -> " <> render e2
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
    Case e alts -> "if " <> render e <> " is " <> showAlts where
      showAlts = T.intercalate " | " $ map showAlt alts
      showAlt (p, e) = render p <> " -> " <> render e
    e -> pack $ show e
  renderParens e = case e of
    Apply _ _ -> parens
    Typed _ _ -> parens
    Lambda _ _ -> parens
    _ -> render e
    where parens = "(" <> render e <> ")"

instance IsString Interp where
  fromString = Plain . fromString

instance Render Interp where
  render interp = "\"" <> go interp <> "\"" where
    go (Plain text) = text
    go (Interp in1 expr in2) = go in1 <> "$(" <> render expr <> ")" <> go in2

instance Monoid Interp where
  mempty = Plain ""
  mappend (Plain s1) (Plain s2) = Plain (s1 <> s2)
  mappend interp (Interp in1 e in2) = Interp (mappend interp in1) e in2
  mappend (Interp in1 e in2) interp = Interp in1 e (mappend in2 interp)


instance Semigroup Interp where
  (<>) = mappend

addChar :: Interp -> Char -> Interp
addChar interp c = interp <> Plain (singleton c)

-- | Characters legal in symbols.
symChars :: P.String
symChars = "+-/*:|&^%$><"

-- | A binary expression is syntactic sugar for a nested application.
binary :: Expr -> Name -> Expr -> Expr
binary e1 op e2 = Apply (Apply (Variable op) e1) e2

-- | Tests if the string is an operator (symbol).
isOp :: Text -> Bool
isOp = T.all (`S.member` (S.fromList symChars))

-- | "Unrolls" an application into its left-most function and its arguments.
unroll :: Expr -> (Expr, [Expr])
unroll (Apply a b) = let (f, xs) = unroll a in (f, xs `snoc` b)
unroll e = (e, [])


instance FreeVars Interp where
  freevars (Interp i1 e i2) = freevars i1 <> freevars e <> freevars i2
  freevars _ = mempty

instance FreeVars Expr where
  freevars = \case
    Variable name -> S.singleton name
    String interp -> freevars interp
    Typed e _ -> freevars e
    Lambda name e -> S.delete name $ freevars e
    Case e alts -> freevars e <> freeAlts where
      freeAlts = concatMap (\(p, e) -> freevars e \\ freevars p) alts
    Let name e1 e2 -> freevars e1 <> (S.delete name $ freevars e2)
    Apply e1 e2 -> freevars e1 <> freevars e2
    Dot e _ -> freevars e
    List es -> concatMap freevars es
    Record es -> concatMap freevars es
    If e1 e2 e3 -> freevars e1 <> freevars e2 <> freevars e3
