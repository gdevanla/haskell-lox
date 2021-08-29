{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
import Import
import System.Environment
import System.IO (print)
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_haskell_lox

import Data.Char
import FunctionsAndTypesForParsing (parseWithEof, parseWithLeftOver, regularParse)
import Text.Parsec.String as PS
-- import Text.Parsec.String.Char
import Text.Parsec.String.Char (anyChar)
import Text.Parsec.String.Combinator (many1)

import Text.Parsec as P
import RIO.Partial (read)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [s] -> either print print $ parse myParser "" s
    _ -> error "please pass one argument with the string to parse"


num1 :: PS.Parser Integer
num1 = do
  digits <- P.many digit
  return (read digits)

var :: PS.Parser String
var = do
  fc <- firstChar
  rest <- P.many nonFirstChar
  return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isLetter a || a == '_' || isDigit a)

data Parentheses = Parentheses Integer
  deriving (Eq, Show)

parens :: PS.Parser Parentheses
parens = do
  void $ char '('
  e <- P.many1 digit
  void $ char ')'
  return (Parentheses (read e))

data SingleAdd = SingleAdd Integer Integer
  deriving (Eq, Show)

add:: PS.Parser SingleAdd
add = do
  f <- P.many1 digit
  void $ whitespace
  void $ char '+'
  void $ whitespace
  s <- P.many1 digit
  return $ SingleAdd (read f) (read s)

whitespace:: PS.Parser ()
whitespace = void $ P.many $ oneOf " \n\t"

lexeme :: PS.Parser a -> PS.Parser a
lexeme p = do
  x <- p
  whitespace
  return x

parseWithWhiteSpace :: PS.Parser a -> String -> Either P.ParseError a
parseWithWhiteSpace p = parseWithEof wrapper
  where
    wrapper = do
      whitespace
      lexeme p

data SimpleExpr
  = Num Integer
  | Var String
  | Add SimpleExpr SimpleExpr
  | Parens SimpleExpr
  deriving (Eq, Show)


numE :: PS.Parser SimpleExpr
numE = do
  n <- lexeme $ P.many1 digit
  return $ Num $ read n

varE :: PS.Parser SimpleExpr
varE = lexeme $ do
  fc <- firstChar
  rest <- P.many nonFirstChar
  return $ Var (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parensE' :: PS.Parser SimpleExpr
parensE' = do
  void $ lexeme $ char '('
  e <- numE
  void $ lexeme $ char ')'
  return $ Parens e

addE :: PS.Parser SimpleExpr
addE = do
  e0 <- numE
  void $ lexeme $ char '+'
  e1 <- numE
  return $ Add e0 e1

numOrVar :: PS.Parser SimpleExpr
numOrVar = numE P.<|> varE

simpleExpr :: PS.Parser SimpleExpr
simpleExpr = P.try addE P.<|> numE P.<|> varE P.<|> parensE'

parensE3 :: PS.Parser SimpleExpr
parensE3 = do
  void $ lexeme $ char '('
  e <- simpleExpr3
  void $ lexeme $ char ')'
  return $ Parens e

addE3 :: PS.Parser SimpleExpr
addE3 = do
  e0 <- simpleExpr3
  void $ lexeme $ char '+'
  e1 <- simpleExpr3
  return $ Add e0 e1

simpleExpr3 :: PS.Parser SimpleExpr
simpleExpr3 = P.try addE3 P.<|> numE P.<|> varE P.<|> parensE3

myParser  :: PS.Parser ()
myParser = void $ string "correct"
