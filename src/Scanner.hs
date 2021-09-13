{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scanner where

import           Data.Char
import           Data.Text                     as T
import           Import                  hiding ( (<|>)
                                                , many
                                                , try
                                                )
import           RIO.Partial                    ( read )
import           Text.Parsec
import           Text.Parsec.Char              as PC
import           Text.Parsec.String            as PS

data LoxObject = JString | JDouble
  deriving (Show, Eq)

data Op = And | Or | Plus | Minus | Eq | Neq | Leq | Geq | GT | LT
  deriving (Show, Eq)

data LoxTok
  = -- Single-character tokens.
    LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Semicolon
  | Slash
  | Star
  | -- One or two character tokens.
    Bang
  | Equal
  | Greater
  | Less
  | LessEqual
  | -- Literals.
    Identifier String
  | String String
  | Number Double
  | Comment Text
   -- Keywords.
  | Class
  | Else
  | BoolTok Bool
  | OpTok Op
  | Fun
  | For
  | If
  | Nil
  | Print
  | Return
  | Super
  | This
  | Var
  | While
  | Whitespace
  | Eof
  deriving (Show, Eq)

data LoxTokInfo = LoxTokInfo
  { tok             :: LoxTok
  , tokinfo_lexeme  :: Maybe T.Text
  , tokinfo_literal :: Maybe LoxObject
  , position        :: SourcePos
  }
  deriving (Show, Eq)

tokenShow :: LoxTokInfo -> String
tokenShow t = "LoxTok=" ++ show (tok t)

type LoxScannerResult = Either ParseError [LoxTokInfo]
-- type LoxScanner = Parsec String () [LoxTok]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

whitespace1 :: Parser LoxTokInfo
whitespace1 = withInfo $ Whitespace <$ many1 (char ' ')

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

withInfo :: Parser LoxTok -> Parser LoxTokInfo
withInfo p = do
  source_pos <- getPosition
  sel        <- p
  pure $ LoxTokInfo sel Nothing Nothing source_pos

-- whitespaceToken1 :: Parser LoxTokInfo
-- whitespaceToken1 = do
--   source_pos <- getPosition
--   pure $ LoxTokInfo WHITESPACE Nothing Nothing source_pos

lSingleChar :: Parser LoxTokInfo
lSingleChar =
  withInfo
    .   choice
    $   build
    <$> [ (LParen     , '(')
        , (RParen     , ')')
        , (LBrace     , '{')
        , (LBrace     , '}')
        , (Comma      , ',')
        , (Dot        , '.')
        , (OpTok Minus, '-')
        , (OpTok Plus , '+')
        , (Semicolon  , ';')
        , (Slash      , '/')
        , (Star       , '*')
        , (Bang       , '!')
        , (Equal      , '=')
        , (Greater    , '>')
        , (Less       , '<')
        ]
 where
  build :: (LoxTok, Char) -> Parser LoxTok
  build (x, y) = lexeme $ x <$ char y

build :: (LoxTok, String) -> Parser LoxTok
build (x, y) = lexeme $ x <$ string y

lOp :: Parser LoxTok
lOp =
  choice
    $   build
    <$> [ (OpTok Neq, "!=")
        , (OpTok Eq , "==")
        , (OpTok Geq, ">=")
        , (OpTok Leq, "<=")
        ]

lDoubleTok :: Parser LoxTokInfo
lDoubleTok = do
  source_pos <- getPosition
  sel        <- lOp
  pure $ LoxTokInfo sel Nothing Nothing source_pos

keywordMap :: [(LoxTok, String)]
keywordMap =
  [ (OpTok And    , "and")
  , (Class        , "class")
  , (Else         , "else")
  , (BoolTok False, "false")
  , (Fun          , "fun")
  , (For          , "for")
  , (If           , "if")
  , (Nil          , "nil")
  , (OpTok Or     , "or")
  , (Print        , "print")
  , (Return       , "pure")
  , (Super        , "super")
  , (This         , "this")
  , (BoolTok True , "true")
  , (Var          , "var")
  , (While        , "while")
  ]

scanDouble :: Parser LoxTokInfo
scanDouble = withInfo $ do
  preDot     <- Text.Parsec.many1 digit
  postDotMay <- Text.Parsec.optionMaybe $ do
    dot      <- char '.'
    decimals <- Text.Parsec.many1 digit
    pure $ dot : decimals
  pure $ Number $ read $ preDot <> fromMaybe "" postDotMay

-- -- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
escape :: Parser String
escape = liftA2 (:) (char '\\') (pure <$> oneOf "\\\"0nrvtbf") -- all the characters which can be escaped

lBetween :: Parser l -> Parser a -> Parser r -> Parser a
lBetween l p r = lexeme $ l *> p <* r

scanQuotedString :: Parser LoxTokInfo
scanQuotedString = withInfo lString
 where
  lString =
    String . Import.concat <$> lBetween (char '"') (many lStringChar) (char '"')
  lStringChar = nonEscape <|> escape
  nonEscape   = pure <$> noneOf "\\\"\0\n\r\v\t\b\f"

-- -- http://jakewheat.github.io/intro_to_parsing/#_var
var :: Parser String
var = liftA2 (:) firstChar (many nonFirstChar)
 where
  firstChar    = satisfy (\a -> isLetter a || a == '_')
  nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

lKeywordOrIdent :: Parser LoxTokInfo
lKeywordOrIdent = withInfo $ do
  s <- var
  -- A map lookup would make more sense here
  case Import.filter ((== s) . snd) keywordMap of
    []         -> pure (Identifier s)
    (x, _) : _ -> pure x

scanComment :: Parser LoxTokInfo
scanComment = do
  source_pos <- getPosition
  _          <- string "//"
  -- TODO: Find a better way to do this, scanning this more than once is not desirable
  comment    <-
    try (manyTill anyToken (try (oneOf "\n"))) <|> manyTill anyToken eof
  pure $ LoxTokInfo (Comment (T.pack comment)) Nothing Nothing source_pos

scanToken :: Parser LoxTokInfo
scanToken =
  try scanComment
    <|> try lDoubleTok
    <|> try lSingleChar
    <|> try scanQuotedString
    <|> try scanDouble
    <|> lKeywordOrIdent

scanner :: String -> LoxScannerResult
scanner = parse (many scanToken <* eof) ""
