{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase #-}
module Language.Rowling.Parser where

import qualified Prelude as P
import Data.Char (isAlpha)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified GHC.Exts as GHC
import Prelude (String)
import Text.Parsec hiding (many, (<|>), spaces, parse)
import qualified Text.Parsec as Parsec

import Language.Rowling.Common hiding (try)
import Language.Rowling.Definitions

-- | The parser state, if any.
type ParserState = ()

-- | The parser type.
type Parser = ParsecT String ParserState Identity

------------------------------------------------------------------------------
-- * Basics
------------------------------------------------------------------------------

-- | Set of keywords.
keywords :: Set Text
keywords = S.fromList ["if", "then", "else", "is", "true", "false",
                       "let", "with", "without"]

-- | Set of reserved symbols.
keysymbols :: Set Text
keysymbols = S.fromList ["->", "::", ":", "|", "="]

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
lowerIdent = lexeme lowerIdent'

-- | Parses an identifier, but doesn't consume trailing whitespace.
lowerIdent' :: Parser Text
lowerIdent' = do
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
  else unexpected $ show sym <> " is not a " <> show s

-- | Parses any non-reserved symbol.
symbol :: Parser Text
symbol = lexeme $ try $ do
  sym <- fmap pack $ many1 $ oneOf symChars
  if sym `member` keysymbols then unexpected $ "keysymbol " <> show sym
  else return sym

-- | Parses a non-keyword lower-case-starting identifier.
identifier :: Parser Text
identifier = notKeyword lowerIdent

-- | Parses anything that can be in a keypath, including digits.
keyPathVar :: Parser Text
keyPathVar = anyIdentifier <|> fmap render pInt

-- | Parses any identifier (upper- or lower-case).
anyIdentifier :: Parser Text
anyIdentifier = upperIdent <|> lowerIdent

------------------------------------------------------------------------------
-- * Strings and interpolated strings
------------------------------------------------------------------------------

pString :: Parser Expr
pString = String . Plain <$> pBasicString

-- | Parses a string constant, without interpolation.
pBasicString :: Parser Text
pBasicString = do
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

-- | Parses an interpolated string, NOT in quotes.
pInterp :: Parser Interp
pInterp = do
  plain <- fromString <$> many (noneOf "$")
  option plain $ do
    char '$'
    lookAhead anyChar >>= \case
      -- If it's a letter, grab a variable.
      c | isAlpha c -> Interp plain <$> dots <*> pInterp
      -- If it's an open parens, grab what's in the parens.
      '(' -> Interp plain <$> parens <*> pInterp
      -- If there's a backslash, we're escaping whatever's next.
      '\\' -> do c <- anyChar >> anyChar
                 map ((plain `addChar` c) <>) pInterp
      -- Otherwise, just keep going and append what we have.
      _ -> map (plain <>) pInterp
  where dots = map Variable lowerIdent' >>= getDots
        getDots expr = do
          -- See if there's a period, AND that there is a letter immediately
          -- following the period. If so, grab another identifier; otherwise
          -- return what we have so far.
          option expr $ try $ do
            spaces >> char '.'
            getDots =<< Dot expr <$> lowerIdent'
        parens = between (schar '(') (char ')') pExpr


------------------------------------------------------------------------------
-- * Expressions
------------------------------------------------------------------------------

-- | Parses an identifier and wraps in a `Variable` expression.
pVariable :: Parser Expr
pVariable = choice [Variable <$> lower, Constructor <$> upperIdent] where
  lower = notKeyword lowerIdent

-- | Parses a number (int or float).
pNumber :: Parser Expr
pNumber = lexeme $ choice [Float <$> pFloat, Int <$> pInt]

-- | Basic expression terms.
pTerm :: Parser Expr
pTerm = choice [pNumber, pString, pVariable, pParens, pList]

-- | A binary expression, lambda, or let. Can be annotated with a type.
pExpr :: Parser Expr
pExpr = do e <- choice [pBinary, pLambda, pLet, pIf]
           option e $ Typed e <$> (keysymbol "::" *> pType)

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
  bareExpr = try $ pExpr <* notFollowedBy (char '=')
  getKeyVals = keyVal `sepEndBy` schar ','
  keyVal = (,) <$> identifier <*> (sstring "=" *> pExpr)

pList :: Parser Expr
pList = grab $ between (schar '[') (schar ']') $ pExpr `sepBy` schar ','
  where grab = map GHC.fromList

-- | Creates a "tuple" (record with integer fields) from an expression list.
makeTuple :: [Expr] -> [(Text, Expr)]
makeTuple es = map (\(i, e) -> (render i, e)) $ zip [0 :: Int ..] es

-- | A let statement.
pLet :: Parser Expr
pLet = keyword "let" >> do
  expr <- pBinary
  body <- schar '=' *> pExpr
  case unroll expr of
    -- A single, pattern argument; this is a deconstruction. For example,
    -- we can rewrite `let Just x = y; z` as `if z is Just x -> y`.
    (x, _) | not (isVariable x) -> do
      y <- rest
      return $ Case body [(expr, y)]
    -- A variable on the left means this is a variable definition.
    (Variable name, args) ->
      if all isVariable args then do
        -- If all of the arguments are also variables, or if there are no
        -- arguments, then we can just use a simple fold to make a lambda.
        -- This also covers the no-argument case.
        let names = map (\(Variable v) -> v) args
        Let name (foldr Lambda body names) <$> rest
      else do
        -- If there are restrictions in the arguments, then we need to
        -- construct a case statement. And there might be multiple patterns.
        -- So use `getOthers` to collect them.
        let numArgs = length args
        others <- option [] $ schar '|' *> getOthers name numArgs
        -- Our full alternatives list is the original pattern and body joined
        -- onto the others.
        let alts = (toPattern args, body) : others
        -- Create a names list, one for each argument.
        let params = take numArgs $ map singleton ['a'..]
            finalBody = Case (toPattern $ map Variable params) alts
        Let name (foldr Lambda finalBody params) <$> rest
  where rest = schar ';' *> pExpr
        isVariable (Variable _) = True
        isVariable _ = False

-- | Converts multiple patterns into a single pattern, using a list if there
-- is not exactly one element in the argument.
toPattern :: [Pattern] -> Pattern
toPattern exprs = case exprs of [p] -> p; _ -> List $ fromList exprs

-- | Gets additional alternatives. After grabbing a single function
-- definition, there can be 0 or more additional alternatives for this
-- function. For example:
-- >>> let fact 1 = 0 | fact n = n * fact (n - 1);
-- In this case, the number of arguments would be 1, and the function name
-- would be @fact@.
getOthers :: Name -- ^ The name of the function.
          -> Int -- ^ The number of arguments to grab (must be same each time)
          -> Parser [(Pattern, Expr)] -- ^ A list of alternatives.
getOthers funcName numArgs = do
  -- Get the pattern on the left side.
  pattern <- unroll <$> pBinary >>= \case
    (Variable f, es) -> do
      -- Make sure the function name matches.
      when (f /= funcName) $ do
        unexpected $ "Expected function named " <> show funcName
      -- Make sure the same number of arguments have been declared.
      when (length es /= numArgs) $ do
        unexpected $ "Wrong number of arguments, expected " <> show numArgs
      return $ toPattern es
  -- Get the definition.
  body <- schar '=' *> pExpr
  -- Continue if there's a pipe.
  option [(pattern, body)] $ schar '|' >> do
    ((pattern, body):) <$> getOthers funcName numArgs



-- | An if statement.
pIf :: Parser Expr
pIf = keyword "if" >> do
  cond <- pExpr
  getIs cond <|> getThen cond
  where
    alts = tuple pBinary (keysymbol "->" *> pExpr) `sepBy1` schar '|'
    getIs cond = map (Case cond) $ (keyword "is" *> alts)
    getThen cond = If cond <$> (keyword "then" *> pExpr)
                           <*> (keyword "else" *> pExpr)
-- | An expression with a `.` and a field name.
pDot :: Parser Expr
pDot = pTerm >>= getNext where
  getNext expr = option expr $ do
    next <- schar '.' *> keyPathVar
    getNext $ Dot expr next

-- | An expression applied to another expression.
pApply :: Parser Expr
pApply = pDot `chainl1` (pure Apply)

-- | A lambda expression.
pLambda :: Parser Expr
pLambda = schar '&' >> do
  let alt = tuple pTerm (keysymbol "->" *> pExpr)
  alt `sepBy1` schar '|' >>= \case
    [(Variable name, body)] -> return $ Lambda name body
    alts -> return $ Lambda "x" $ Case "x" alts

-- | Two expressions joined by a binary operator.
pBinary :: Parser Expr
pBinary = pApply `chainl1` fmap (flip binary) symbol

------------------------------------------------------------------------------
-- * Types
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
-- * Running the parser
------------------------------------------------------------------------------

-- | Parse a string as an expression, or return an error.
parseIt :: String -> Either ParseError Expr
parseIt = parse (pExpr <* eof)

-- | Parse a string as a type, or return an error.
parseType :: String -> Either ParseError Type
parseType = parse (pType <* eof)

parseInterp :: String -> Either ParseError Interp
parseInterp = parse pInterp

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""
