{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scanner(LoxTok(..), LoxTokInfo(..), scanner) where

import Import hiding (many, try, (<|>))
import Data.Text as T
import Data.Char
import Text.Parsec.String as PS
import Text.Parsec.Char as PC
import Text.Parsec
import RIO.Partial (read)

data LoxObject = JString | JDouble
  deriving (Show, Eq)

data LoxTok
  = -- Single-character tokens.
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | -- One or two character tokens.
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Literals.
    IDENTIFIER String
  | STRING String
  | NUMBER Double
  | COMMENT Text
   -- Keywords.
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | WHITESPACE
  | EOF
  deriving (Show, Eq)

data LoxTokInfo = LoxTokInfo
  { tokinfo_type :: LoxTok,
    tokinfo_lexeme :: Maybe T.Text,
    tokinfo_literal :: Maybe LoxObject,
    tok_position :: SourcePos
  }
  deriving (Show, Eq)


--tokenShow :: LoxTokInfo -> String
--tokenShow t = "LoxTok=" ++ show (tokinfo_type t)

type LoxScannerResult = Either ParseError [LoxTokInfo]
-- type LoxScanner = Parsec String () [LoxTok]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
-- whitespaceToken1 :: Parser LoxTokInfo
-- whitespaceToken1 = do
--   source_pos <- getPosition
--   return $ LoxTokInfo WHITESPACE Nothing Nothing source_pos

whitespaceToken :: Parser LoxTokInfo

charMapping :: [(LoxTok, Char)]
charMapping =
  [ (LEFT_PAREN, '('),
    (RIGHT_PAREN, ')'),
    (LEFT_BRACE, '{'),
    (RIGHT_BRACE, '}'),
    (COMMA, ','),
    (DOT, '.'),
    (MINUS, '-'),
    (PLUS, '+'),
    (SEMICOLON, ';'),
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
  sel <- choice $ build <$> charMapping
  return $ LoxTokInfo sel Nothing Nothing source_pos
  where
    build :: (LoxTok, Char) -> Parser LoxTok
    build (x, y) = x <$ char y <* whitespace

doubleCharMapping :: [(LoxTok, String)]
doubleCharMapping =
  [ (BANG_EQUAL, "!="),
    (EQUAL_EQUAL, "=="),
    (GREATER_EQUAL, ">="),
    (LESS_EQUAL, "<=")
  ]

scanDoubleToken :: Parser LoxTokInfo
scanDoubleToken = do
  source_pos <- getPosition
  sel <- choice $ build <$> doubleCharMapping
  return $ LoxTokInfo sel Nothing Nothing source_pos
  where
    build :: (LoxTok, String) -> Parser LoxTok
    build (x, y) = x <$ string y <* whitespace

keywordMapping :: [(LoxTok, String)]
keywordMapping =
  [ (AND, "and"),
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

-- scanKeywordToken :: Parser LoxTokInfo
-- scanKeywordToken = do
--   source_pos <- getPosition
--   sel <- choice $ build <$> keywordMapping
--   return $ LoxTokInfo sel Nothing Nothing source_pos
--   where
--     build :: (LoxTok, String) -> Parser LoxTok
--     build (x, y) = x <$ string y <* whitespace

whitespaceToken = do
  source_pos <- getPosition
  _ <- many1 $ char ' '
  return $ LoxTokInfo WHITESPACE Nothing Nothing source_pos

scanDouble :: Parser LoxTokInfo
scanDouble = do
  source_pos <- getPosition
  let la = lookAhead (whitespaceToken <|> scanSingleCharToken)
  sel <- do
    firstPart <- Text.Parsec.many1 digit
    try (secondCharacter firstPart <* la <* whitespace) <|> NUMBER (read firstPart) <$ la <* whitespace
  return $ LoxTokInfo sel Nothing Nothing source_pos
  where
    secondCharacter :: String -> Parser LoxTok
    secondCharacter firstPart = do
      void $ char '.'
      secondPart <- Text.Parsec.many1 digit <* whitespace
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
  qstring <- char '"' *> many character <* char '"' <* whitespace
  return $ LoxTokInfo (STRING $ Import.concat qstring) Nothing Nothing source_pos

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
  s <- var <* whitespace
  result ([(x, y) | (x, y) <- keywordMapping, y == s]) s source_pos
  where
    result xs s source_pos = do
      case xs of
        [] -> return $ LoxTokInfo (IDENTIFIER s) Nothing Nothing source_pos
        (x, _) : _ -> return $ LoxTokInfo x Nothing Nothing source_pos

scanComment :: Parser LoxTokInfo
scanComment = do
  source_pos <- getPosition
  _ <- string "//"
  -- TODO: Find a better way to do this, scanning this more than once is not desirable
  comment <- try (manyTill anyToken (try (oneOf "\n"))) <|> manyTill anyToken eof
  return $ LoxTokInfo (COMMENT (T.pack comment)) Nothing Nothing source_pos

scanToken :: Parser LoxTokInfo
scanToken =
  try scanComment
    <|> try scanDoubleToken
    <|> try scanSingleCharToken
    <|> try scanQuotedString
    <|> scanDouble
    <|> checkIfIdentifier

scanner :: String -> LoxScannerResult
scanner = parse (many scanToken <* eof) ""
