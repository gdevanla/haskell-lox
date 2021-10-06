module PrattParser where

import Control.Monad.State.Strict
import Text.Parsec.String
import Text.Parsec.Char
import qualified Text.Parsec as PS
import Text.Parsec.Combinator
import Control.Applicative
import Data.Either


whitespace :: Parser ()
whitespace = void $ PS.many $ oneOf " "

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

scanNumber :: Parser Token
scanNumber = do
  digits <- lexeme $ PS.many1 digit
  return $ Number (read digits)

scanOperator :: Parser Token
scanOperator = choice $ build <$> [
  (Plus, '+'),
  (Minus, '-'),
  (Slash, '/'),
  (Star, '*'),
  (Exp, '^'),
  (LParen, '('),
  (RParen, ')')
  ]
  where
    build :: (Token, Char) -> Parser Token
    build (x, y) = lexeme $ x <$ char y

parseExpression :: String -> Either PS.ParseError [Token]
parseExpression inp = do
  toks <- PS.parse (many1 (PS.try scanNumber <|> scanOperator) <* eof) "" inp
  return $ toks ++ [EndTok]


data Token =
  Plus
  | Minus
  | Star
  | Slash
  | Exp
  | Number !Double
  | LParen
  | RParen
  | EndTok
  deriving (Eq, Show)


type TokenS = State [Token] Double

nextToken :: State [Token] Token
nextToken = do
  s <- get
  case s of
    (x:xs) -> do
      put xs
      return x
    _ -> error "Token list is empty"

currToken :: State [Token] Token
currToken = do
  s <- get
  case s of
    (x:_) -> return x
    _ -> error "No more tokens"

hasToken :: State [Token] Bool
hasToken = do
  s <- get
  case s of
    (_:_) -> return True
    [] -> return False


nud :: Token -> State [Token] Double
nud (Number x) = return x
nud Minus = do
  right <- expression 100  -- add this to prefix map
  return $ -right
nud LParen = do
  right <- expression 0
  token <- currToken
  case token of
    RParen -> do
      void nextToken
      return right
    _ -> error $ "unexpected token = " ++ show token ++ " found."
nud _ = error "only literal supported for nud"

prec :: Token -> Double
prec tok = case tok of
  Number _ -> 0
  LParen -> 0
  RParen -> 0
  EndTok -> 0
  Minus -> 10
  Plus -> 10
  Star -> 20
  Slash -> 20
  Exp -> 30
  _ -> error $ "prec not defined for = " ++ show tok

led :: Double -> Token -> TokenS
led left tok = do
  case tok of
    Plus -> do
      right <- expression (prec tok)
      return $ left + right
    Minus -> do
      right <- expression (prec tok)
      return $ left - right
    Star -> do
      right <- expression (prec tok)
      return $ left * right
    Slash -> do
      right <- expression (prec tok)
      return $ left / right
    Exp -> do
      right <- expression $ prec tok - 1
      return $ left ** right
    _ -> error $ show tok ++ "not supported"


expression :: Double -> TokenS
expression rbp = do
  token <- nextToken
  left <- nud token
  nt1 <- currToken
  go left nt1
  where
    go left' nt' = if rbp < prec nt' then do
        void nextToken
        left'' <- led left' nt'
        nt'' <- currToken
        go left'' nt''
      else return left'

-- expr1 = [(Number 1), Plus, (Number 2), Plus, (Number 3), Plus, (Number 4), Plus, (Number 5), EndTok]
-- expr2 = [(Number 10), Minus, (Number 20), Plus, (Number 10), Plus, (Number 20), EndTok]
-- expr3 = [(Number 10), Star, (Number 20), Star, (Number 10), Slash, (Number 5), EndTok]
-- expr4 = [(Number 1), Plus, (Number 2), Star, (Number 3), Plus, (Number 10), Slash, (Number 5), EndTok]
-- expr5 = [(Number 1), Plus, (Number 2), Star, (Number 3), Plus, (Number 10), Slash, Minus, (Number 5), EndTok]
-- expr6 = [(Number 3), Exp, (Number 2), Exp, (Number 3), EndTok]
-- expr7 = [LParen, (Number 1), Plus, (Number 2), RParen, Star, (Number 3), Plus, (Number 10), Slash, (Number 5), EndTok]

-- evalExpression = map (runState (expression 0)) [expr1, expr2, expr3, expr4, expr5, expr6, expr7]

evalExpression exprs = map (evalState (expression 0) . fromRight [] . parseExpression) exprs

evalAll = evalExpression [
  "1+2+3+4",
  "10-2+1",
  "10-5-1",
  "10+2*3-8",
  "3^2^3",
  "(10+2)*3-8"
  ]
