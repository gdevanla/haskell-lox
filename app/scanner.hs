{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
import Import hiding (many, (<|>), try)
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
-- import Text.Parsec.String.Char (anyCh
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
  IDENTIFIER| STRING String| NUMBER String|

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
 (STAR, '*'),
 (BANG, '!'),
 (EQUAL, '='),
 (GREATER, '>'),
 (LESS, '<')
 ]

scanDoubleToken :: Parser LoxTok
scanDoubleToken = choice $ build <$> double_char_mapping
  where
    build :: (LoxTok, String) -> Parser LoxTok
    build (x, y) = x <$ string y

double_char_mapping :: [(LoxTok, String)]
double_char_mapping =
  [(BANG_EQUAL, "!="),
  (EQUAL_EQUAL, "=="),
  (GREATER_EQUAL, ">="),
  (LESS_EQUAL, "<=")
  ]

keyword_mapping :: [(LoxTok, String)]
keyword_mapping =
  [
    (AND, "and"),
    (CLASS, "class"),
    (ELSE, "else"),
    (FALSE, "false"),
    (FUN, "fun"),
    (FOR, "for"),
    (IF, "if"),
    (NIL, "nil"),
    (OR, "or"),
    (PRINT, "print"),
    (RETURN, "return"),
    (SUPER, "super"),
    (THIS, "this"),
    (TRUE, "true"),
    (VAR, "var"),
    (WHILE, "while")
    ]

scanKeywordToken :: Parser LoxTok
scanKeywordToken = choice $ build <$> keyword_mapping
  where
    build :: (LoxTok, String) -> Parser LoxTok
    build (x, y) = x <$ string y

-- https :// stackoverflow . com / questions / 24106314 / parser - for - quoted - string - using - parsec
escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

scanQuotedString :: Parser LoxTok
scanQuotedString = do
  char '"'
  strings <- many character
  char '"'
  return $ STRING $ Import.concat strings

scanDouble :: Parser LoxTok
scanDouble = do
  firstPart <- Text.Parsec.many1 digit
  try (secondCharacter firstPart) <|> return (NUMBER firstPart)
  where
    secondCharacter :: String -> Parser LoxTok
    secondCharacter firstPart = do
      void $ char '.'
      secondPart <- Text.Parsec.many1 digit
      return $ NUMBER $ Import.concat [firstPart, ".", secondPart]


scanToken :: Parser LoxTok
scanToken =
  try scanDoubleToken <|>
  try scanSingleCharToken <|>
  try scanKeywordToken <|>
  try scanQuotedString <|>
  try scanDouble
