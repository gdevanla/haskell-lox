{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

import RIO
import System.IO (print)


import Control.Applicative (many, (*>), (<$), (<$>), (<*), (<*>), (<|>))
import Control.Monad (ap, void)
import Data.Char (isDigit, isLetter)
import FunctionsAndTypesForParsing

import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (char, digit, letter, oneOf, satisfy)
import Text.Parsec.String.Combinator (chainl1, many1)
import RIO.Partial (read)

data SimpleExpr
  = Num Integer
  | Var String
  | Add SimpleExpr SimpleExpr
  | Parens SimpleExpr
  deriving (Eq, Show)

myParser1 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
myParser1 ctor pa pb = ctor <$> pa <*> pb

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexemeD :: Parser a -> Parser a
lexemeD p = do
  x <- p
  void whitespace
  return x

lexemeA0 :: Parser a -> Parser a
lexemeA0 p = p <* whitespace

numD :: Parser SimpleExpr
numD = do
  n <- lexemeD $ many1 digit
  return $ Num $ read n

numA :: Parser SimpleExpr
numA = Num . read <$> lexemeD (many1 digit)


varD :: Parser SimpleExpr
varD = lexemeA0 $ do
  fc <- firstChar
  rest <- many nonFirstChar
  return $ Var (fc : rest)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

-- varD1 :: Parser SimpleExpr
-- varD1 = lexemeA0 $
--   return $ Var <$> ((:) <$> firstChar <*> (many nonFirstChar))
--   where
--     firstChar = letter <|> char '_'
--     nonFirstChar = digit <|> firstChar

parensD :: Parser SimpleExpr
parensD = do
  void $ lexemeA0 $ char '('
  e <- simpleExprD
  void $ lexemeA0 $ char ')'
  return $ Parens e

parensA :: Parser SimpleExpr
parensA =  Parens <$> (lexemeA0 (char '(') *> simpleExprD <* lexemeA0 (char ')'))

termD :: Parser SimpleExpr
termD = numD <|> varD <|> parensD

simpleExprD :: Parser SimpleExpr
simpleExprD = chainl1 termD op
  where
    op = do
      void $ lexemeA0 $ char '+'
      return Add


main :: IO ()
main = print "hello"
