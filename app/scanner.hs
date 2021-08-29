{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
import Import hiding (many, (<|>))
import System.Environment
import System.IO (print)
import Run
import RIO.Process
--import Options.Applicative.Simple
import qualified Paths_haskell_lox

import Data.Text as T
import Data.Char
import FunctionsAndTypesForParsing (parseWithEof, parseWithLeftOver, regularParse)
import Text.Parsec.String as PS
import Text.Parsec.Char as PC
-- import Text.Parsec.String.Char
import Text.Parsec.String.Char (anyChar)
import Text.Parsec.String.Combinator (many1)

import Text.Parsec
import RIO.Partial (read)

main :: IO ()
main = return ()
  -- a <- getArgs
  -- case a of
  --   [s] -> either print print $ parse myParser "" s
  --   _ -> error "please pass one argument with the string to parse"

data LoxObject = JString | JDouble

data LoxTok =
  -- Single-character tokens.
  LEFT_PAREN| RIGHT_PAREN| LEFT_BRACE| RIGHT_BRACE|
  COMMA| DOT| MINUS| PLUS| SEMICOLON| SLASH| STAR|

  -- One or two character tokens.
  BANG| BANG_EQUAL|
  EQUAL| EQUAL_EQUAL|
  GREATER| GREATER_EQUAL|
  LESS| LESS_EQUAL|

  -- Literals.
  IDENTIFIER| STRING| NUMBER|

  -- Keywords.
  AND| CLASS| ELSE| FALSE| FUN| FOR| IF| NIL| OR|
  PRINT| RETURN| SUPER| THIS| TRUE| VAR| WHILE|

  EOF
  deriving (Show, Eq)

data LoxTokInfo = LoxTokInfo {
  token_type:: LoxTok,
  lexeme:: T.Text,
  literal:: LoxObject,
  start_pos:: (Int, Int),
  end_pos:: (Int, Int)
  }


type LoxScanner = Parser
-- type LoxScanner = Parsec String () [LoxTok]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme_scan :: LoxScanner a -> LoxScanner a
lexeme_scan p = p <* whitespace


-- token :: Char -> LoxScanner LoxTok

lessThan :: Parser LoxTok
lessThan = (return LEFT_PAREN) <* (PC.char '<')

greaterThan :: Parser LoxTok
greaterThan = (return LEFT_PAREN) <* (PC.char '>')


scanSingleCharToken :: Parser LoxTok
scanSingleCharToken = choice $ build <$> char_mapping
  where
    build :: (LoxTok, Char) -> Parser LoxTok
    build (x, y) = x <$ char y


char_mapping :: [(LoxTok, Char)]
char_mapping =
  [
 (LEFT_PAREN, '('),
 (RIGHT_PAREN, ')'),
 (LEFT_BRACE, '{'),
 (RIGHT_BRACE, '}'),
 (COMMA, ','),
 (DOT, '.'),
 (MINUS, '-'),
 (PLUS, '+'),
 (SEMICOLON, '-'),
 (SLASH, '/'),
 (STAR, '*')
 ]
