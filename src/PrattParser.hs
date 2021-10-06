module PrattParser where

import Control.Monad.State.Strict

data Token =
  Plus
  | Minus
  | Star
  | Slash
  | Number !Double
  | LParen
  | RParen
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


nud :: Token -> Double
nud (Number x) = x
nud _ = error "only literal supported for nud"

prec :: Token -> Int
prec tok = case tok of
  Number _ -> 0
  Minus -> 10
  Plus -> 10
  Star -> 20
  Slash -> 20
  _ -> error "prec not defined"

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
    --(Number x) -> return x
    _ -> error $ show tok ++ "not supported"


expression :: Int -> TokenS
expression rbp = do
  token <- nextToken
  let left = nud token
  h <- hasToken
  if h then do
    nt1 <- currToken
    go left nt1
    else return left
  where
    go left' nt' = if rbp < prec nt' then do
      h <- hasToken
      if h then do
        void nextToken
        left'' <- led left' nt'
        nt'' <- currToken
        go left'' nt''
        else return left'
      else return left'


expr1 = [(Number 1), Plus, (Number 2), Plus,  (Number 3), Plus, (Number 4), Plus, (Number 5), (Number 5)]
expr2 = [(Number 10), Minus, (Number 20), Plus, (Number 10), Plus, (Number 20), (Number 0)]
expr3 = [(Number 10), Star, (Number 20), Star, (Number 10), Slash, (Number 5), (Number 0)]
expr4 = [(Number 1), Plus, (Number 2), Star, (Number 3), Plus, (Number 10), Slash, (Number 5), (Number 0)]

evalExpression expr = runState (expression 0) expr
