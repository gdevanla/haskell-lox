{-# LANGUAGE OverloadedStrings#-}
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
  deriving (Show, Eq)

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
  IDENTIFIER String| STRING String| NUMBER Double|

  -- Keywords.
  AND| CLASS| ELSE| FALSE| FUN| FOR| IF| NIL| OR|
  PRINT| RETURN| SUPER| THIS| TRUE| VAR| WHILE|

  EOF
  deriving (Show, Eq)

data LoxTokInfo = LoxTokInfo {
  tokinfo_type:: LoxTok,
  tokinfo_lexeme:: Maybe T.Text,
  tokinfo_literal:: Maybe LoxObject,
  tok_position:: SourcePos
  }
  deriving (Show, Eq)


type LoxScanner = Parser
-- type LoxScanner = Parsec String () [LoxTok]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme_scan :: LoxScanner a -> LoxScanner a
lexeme_scan p = p <* whitespace


-- token :: Char -> LoxScanner LoxTok

char_mapping :: [(LoxTok, Char)]
char_mapping =
  [ (LEFT_PAREN, '('),
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

scanSingleCharToken :: Parser LoxTokInfo
scanSingleCharToken = do
  source_pos <- getPosition
  sel <- choice $ build <$> char_mapping
  return $ LoxTokInfo sel Nothing Nothing source_pos
    where
      build :: (LoxTok, Char) -> Parser LoxTok
      build (x, y) = x <$ char y <* whitespace

  -- return _ -- $ --LoxTokInfo sel "" "" source_pos

double_char_mapping :: [(LoxTok, String)]
double_char_mapping =
  [ (BANG_EQUAL, "!="),
    (EQUAL_EQUAL, "=="),
    (GREATER_EQUAL, ">="),
    (LESS_EQUAL, "<=")
  ]

scanDoubleToken :: Parser LoxTokInfo
scanDoubleToken = do
  source_pos <- getPosition
  sel <- choice $ build <$> double_char_mapping
  return $ LoxTokInfo sel Nothing Nothing source_pos
  where
    build :: (LoxTok, String) -> Parser LoxTok
    build (x, y) = x <$ string y <* whitespace

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

scanKeywordToken :: Parser LoxTokInfo
scanKeywordToken = do
  source_pos <- getPosition
  sel <- choice $ build <$> keyword_mapping
  return $ LoxTokInfo sel Nothing Nothing source_pos
  where
    build :: (LoxTok, String) -> Parser LoxTok
    build (x, y) = x <$ string y <* whitespace

scanDouble :: Parser LoxTokInfo
scanDouble = do
  source_pos <- getPosition
  sel <- do
    firstPart <- Text.Parsec.many1 digit
    try (secondCharacter firstPart) <|> NUMBER (read firstPart) <$ whitespace
  return $ LoxTokInfo sel Nothing Nothing source_pos
  where
    secondCharacter :: String -> Parser LoxTok
    secondCharacter firstPart = do
      void $ char '.'
      secondPart <- Text.Parsec.many1 digit
      return $ NUMBER $ read $ Import.concat [firstPart, ".", secondPart]

-- -- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

scanQuotedString :: Parser LoxTokInfo
scanQuotedString = do
  source_pos <- getPosition
  string <- char '"' *> many character <* char '"' <* whitespace
  return $ LoxTokInfo (STRING $ Import.concat string) Nothing Nothing source_pos

-- -- http://jakewheat.github.io/intro_to_parsing/#_var
var :: Parser String
var = do
  fc <- firstChar
  rest <- many nonFirstChar
  return (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

checkIfIdentifier :: Parser LoxTokInfo
checkIfIdentifier = do
  source_pos <- getPosition
  s <- var
  result ([(x, y) | (x, y) <- keyword_mapping, y == s]) s source_pos
  where
    result xs s source_pos = do
      case xs of
        [] -> return $ LoxTokInfo (IDENTIFIER s) Nothing Nothing source_pos
        (x, _):_ -> return $ LoxTokInfo x Nothing Nothing  source_pos


scanToken :: Parser LoxTokInfo
scanToken =
  try scanDoubleToken <|>
  try scanSingleCharToken <|>
  try scanQuotedString <|>
  try scanDouble <|>
  checkIfIdentifier

scanner :: String -> Either ParseError [LoxTokInfo]
scanner =  parse (many scanToken <* eof) ""
