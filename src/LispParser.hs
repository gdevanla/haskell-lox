{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LispParser where

import Import hiding (many, (<|>), try)
import Data.Text as T
import Data.Char
import Text.Parsec.Char as PC
import Text.Parsec
import Text.Parsec.String as PS

data LispToken =
  LParen
  | RParen
  | Symbol T.Text
  | String T.Text
  | Numeric T.Text
  | Comma
  deriving (Eq, Show)

-- data LispTokInfo = LispTokInfo LispToken SourcePos

type Lexer = Either ParseError [LispToken]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

charMapping :: [(LispToken, Char)]
charMapping =
  [ (LParen, '('),
    (RParen, ')')
  ]

scanSingleCharToken :: Parser LispToken
scanSingleCharToken = choice $ build <$> charMapping
  where
    build :: (LispToken, Char) -> Parser LispToken
    build (x, y) = x <$ char y <* whitespace


scanSymbol :: Parser LispToken
scanSymbol = do
  fc <- firstChar
  rest <- many nonFirstChar
  void whitespace
  return $ Symbol $ T.pack (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_' || a == '-')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

scanNumeric :: Parser LispToken
scanNumeric = Numeric . T.pack <$> Text.Parsec.many1 digit <* whitespace


lexTokens :: Parser LispToken
lexTokens = try lParens <|> try rParens <|> try scanSymbol <|> scanNumeric
  -- l <- lParens
  -- toks <- try scanSymbolL <|> try scanNumberL <|> try rParens <|> lexTokens
  -- return $ [l] ++ toks
  where
    lParens = LParen <$ satisfy (== '(') <* whitespace
    rParens = RParen <$ satisfy (== ')') <* whitespace

lexer :: String -> Lexer
lexer = parse (many lexTokens) ""


-- Parser functions

newtype Identifier = Identifier {unIdent:: T.Text}
  deriving (Eq, Show)

data Expr
  = ExprLitNum Int
  | ExprVar T.Text
  | ExprLambda [Identifier] Expr
  | ExprApp Expr [Expr]
  | ExprIf !Expr !Expr !Expr
  deriving (Eq, Show)

type ParserResult = Either ParseError Expr
type LispParser a = ParsecT [LispToken] () Identity a

satisfyTok :: (LispToken -> Maybe a) -> LispParser a
satisfyTok f = tokenPrim showTok updateTokPos match
  where
    showTok ti = show ti
    updateTokPos pos _ _ = pos
    match t = f t


satisfyLParen :: LispToken -> Maybe Bool
satisfyLParen LParen = Just True
satisfyLParen _ = Nothing

satisfyRParen :: LispToken -> Maybe Bool
satisfyRParen RParen = Just True
satisfyRParen _ = Nothing

satisfySymbol :: LispToken -> Maybe Expr
satisfySymbol (Symbol s) = Just $ ExprVar s
satisfySymbol _ = Nothing

satisfyIdentifier :: LispToken -> Maybe Identifier
satisfyIdentifier (Symbol s) = Just $ Identifier s
satisfyIdentifier _ = Nothing

satisfyNumeric :: LispToken -> Maybe Expr
satisfyNumeric (Numeric s) = ExprLitNum <$> (readMaybe (T.unpack s)::Maybe Int)
satisfyNumeric _ = Nothing

satisfyLambda :: LispToken -> Maybe Bool
satisfyLambda (Symbol s) = Just $ s == "lambda"
satisfyLambda _ = Nothing

exprVar :: LispParser Expr
exprVar = satisfyTok satisfySymbol

exprLambda :: ParsecT [LispToken] () Identity Expr
exprLambda = do
  void $ satisfyTok satisfyLParen
  void $ satisfyTok satisfyLambda
  void $ satisfyTok satisfyLParen
  ids <- many $ satisfyTok satisfyIdentifier
  void $ satisfyTok satisfyRParen
  expr <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprLambda ids expr

exprApp :: ParsecT [LispToken] () Identity Expr
exprApp = do
  void $ satisfyTok satisfyLParen
  rator <- exprExpr
  expressions <- many exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprApp rator expressions

exprExpr :: ParsecT [LispToken] () Identity Expr
exprExpr = do
  x <- try exprLambda
    <|> try exprApp
    <|> try (satisfyTok satisfyNumeric)
    <|> (satisfyTok satisfySymbol)
  return x


parseExpr :: LispParser Expr
parseExpr = exprExpr

lexAndParse :: String -> ParserResult
lexAndParse s = case lexer s of
  Right toks -> parse parseExpr "" toks
  Left e -> error $ show e
