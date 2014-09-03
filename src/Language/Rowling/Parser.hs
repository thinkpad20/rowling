{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
module Language.Rowling.Parser where

import qualified Prelude as P
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Prelude (String)
import Text.Parsec hiding (many, (<|>), spaces)

import Language.Rowling.Common hiding (try)
import Language.Rowling.Definitions.Types
import Language.Rowling.Definitions.Expressions

-- | The parser state, if any.
type ParserState = ()

-- | The parser type.
type Parser = ParsecT String ParserState Identity

------------------------------------------------------------------------------
-- Basics
------------------------------------------------------------------------------

-- | Set of keywords.
keywords :: Set Text
keywords = S.fromList ["if", "then", "else", "true", "false", "let", "with"]

-- | Set of reserved symbols.
keysymbols :: Set Text
keysymbols = S.fromList ["->", "::", ":", "|"]

-- | Consumes any spaces (not other whitespace).
spaces :: Parser String
spaces = many $ char ' '

-- | Parses the given string and any trailing spaces.
sstring :: String -> Parser String
sstring = lexeme . string

-- | Parses the given character and any trailing spaces.
schar :: Char -> Parser Char
schar = lexeme . char

-- | Parses `p` and any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parses the given string. Does not fail if it's a keyword.
keyword :: String -> Parser String
keyword = try . sstring

-- | Parses an identifier starting with a lower-case letter or underscore.
lowerIdent :: Parser Text
lowerIdent = lexeme $ do
  first <- lower
  rest <- many $ letter <|> digit <|> char '_'
  return $ pack $ first : rest

-- | Parses an identifier starting with an upper-case letter or underscore.
upperIdent :: Parser Text
upperIdent = lexeme $ do
  first <- upper
  rest <- many $ letter <|> digit <|> char '_'
  return $ pack $ first : rest

-- | Parses `p`, but fails if the result is a reserved word.
notKeyword :: Parser Text -> Parser Text
notKeyword p = try $ do
  ident <- p
  if ident `member` keywords then unexpected $ "keyword " <> show ident
  else return ident

-- | Parses an integer.
pInt :: Parser Integer
pInt = P.read <$> many1 digit

-- | Parses a float (must have a dot and digits on both sides).
pFloat :: Parser Double
pFloat = fmap P.read $ try $ do
  first <- many1 digit
  dot <- char '.'
  rest <- many1 digit
  return $ first <> [dot] <> rest

-- | Parses a reserved symbol.
keysymbol :: Text -> Parser Text
keysymbol s = lexeme . try $ do
  sym <- many1 $ oneOf symChars
  if pack sym == s then return s
  else unexpected $ "'" <> sym <> "' is not a '" <> unpack s <> "'"

-- | Parses any non-reserved symbol.
symbol :: Parser Text
symbol = lexeme $ try $ do
  sym <- fmap pack $ many1 $ oneOf symChars
  if sym `member` keysymbols then unexpected $ "keysymbol " <> show sym
  else return sym

-- | Parses a string constant, with \-escaped characters.
pString :: Parser Text
pString = do
  start <- char '"' <|> char '\''
  loop start []
  where
    loop stop acc = do
      anyChar >>= \case
        c | c == stop -> return $ pack $ P.reverse acc
        '\\' -> anyChar >>= \case
          'n' -> escape '\n'
          'r' -> escape '\r'
          't' -> escape '\r'
          'b' -> escape '\r'
          '\\' -> escape '\\'
          '"' -> escape '"'
          '\'' -> escape '\''
          c -> unexpected $ "Unrecognized escape sequence: \\" <> [c]
        c -> escape c
      <|> return (pack $ P.reverse acc)
      where escape c = loop stop (c : acc)

-- | Parses a non-keyword lower-case-starting identifier.
identifier :: Parser Text
identifier = notKeyword lowerIdent

-- | Parses anything that can be in a keypath, including digits.
keyPathVar :: Parser Text
keyPathVar = anyIdentifier <|> fmap render pInt


-- | Parses any identifier (upper- or lower-case).
anyIdentifier :: Parser Text
anyIdentifier = upperIdent <|> lowerIdent

-- | Parses an identifier and wraps in a `Variable` expression.
pVariable :: Parser Expr
pVariable = Variable <$> identifier

-- | Parses a number (int or float).
pNumber :: Parser Expr
pNumber = lexeme $ choice [Float <$> pFloat, Int <$> pInt]

-- | Basic expression terms.
pTerm :: Parser Expr
pTerm = choice [pNumber, String <$> pString, pVariable, pParens]

------------------------------------------------------------------------------
-- Composite pExpressions
------------------------------------------------------------------------------

-- | A binary expression, lambda, or let. Can be annotated with a type.
pExpr :: Parser Expr
pExpr = do e <- pBinary <|> pLambda <|> pLet
           option e $ Typed e <$> (keysymbol "::" *> pType)

-- | An expression with a `.` and a field name.
dot :: Parser Expr
dot = pTerm >>= getNext where
  getNext expr = option expr $ do
    next <- schar '.' *> keyPathVar
    getNext $ Dot expr next

-- | An expression in parentheses. Tuples and record literals are also written
-- this way.
pParens :: Parser Expr
pParens = between (schar '(') (schar ')') getValues where
  getValues = do
    bareExprs <- getBareExprs
    keyVals <- getKeyVals
    case (bareExprs, keyVals) of
      ([e], []) -> return e
      (es, kvs) -> return $ Record $ H.fromList $ makeTuple es <> kvs
  getBareExprs = bareExpr `sepEndBy` schar ','
  bareExpr = try $ pExpr <* notFollowedBy (keysymbol ":")
  getKeyVals = keyVal `sepEndBy` schar ','
  keyVal = (,) <$> identifier <*> (keysymbol ":" *> pExpr)

-- | Creates a "tuple" (record with integer fields) from an expression list.
makeTuple :: [Expr] -> [(Text, Expr)]
makeTuple es = map (\(i, e) -> (render i, e)) $ zip [0 :: Int ..] es

-- | A let statement.
pLet :: Parser Expr
pLet = Let <$> var <*> expr <*> rest where
  var = keyword "let" *> identifier <* schar '='
  expr = pExpr <* schar ';'
  rest = pExpr

-- | An expression applied to another expression.
pApply :: Parser Expr
pApply = dot `chainl1` (pure Apply)

-- | A lambda expression.
pLambda :: Parser Expr
pLambda = Lambda <$> param <*> body where
  param = schar '&' *> pattern
  body = keysymbol "->" *> pExpr
  pattern = pVariable <|> pParens

-- | Two expressions joined by a binary operator.
pBinary :: Parser Expr
pBinary = pApply `chainl1` fmap (flip binary) symbol

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | The top-level type parser.
pType :: Parser Type
pType = pTFunction

-- | One type applied to another.
pTApply :: Parser Type
pTApply = pTTerm `chainl1` (pure TApply)

-- | A type term.
pTTerm :: Parser Type
pTTerm = lexeme $ pTConst <|> pTVar <|> pTParens where
  pTVar = TVar <$> identifier
  pTConst = TConst <$> notKeyword upperIdent

-- | A type wrapped in parentheses. Also how tuples and record types are
-- written.
pTParens :: Parser Type
pTParens = between (schar '(') (schar ')') getTypes where
  getTypes = do
    bareTypes <- getBareTypes
    keyVals <- getKeyVals
    rest <- optionMaybe $ keysymbol "|" *> identifier
    case (bareTypes, keyVals) of
      ([(_, t)], []) | rest == Nothing -> return t
      (ts, kvs) -> return $ TRecord (H.fromList $ ts <> kvs) rest
  getBareTypes' = bareType `sepEndBy` schar ','
  getBareTypes = zip (map render [0 :: Int ..]) <$> getBareTypes'
  bareType = try $ pType <* notFollowedBy (keysymbol ":")
  getKeyVals = keyVal `sepEndBy` schar ','
  keyVal = (,) <$> identifier <*> (keysymbol ":" *> pType)

-- | A function type.
pTFunction :: Parser Type
pTFunction = chainr1 pTApply (keysymbol "->" *> pure (==>))

------------------------------------------------------------------------------
-- Running the parser
------------------------------------------------------------------------------

-- | Parse a string as an expression, or return an error.
parseIt :: String -> Either ParseError Expr
parseIt = parse (pExpr <* eof) ""

-- | Parse a string as a type, or return an error.
parseType :: String -> Either ParseError Type
parseType = parse (pType <* eof) ""
