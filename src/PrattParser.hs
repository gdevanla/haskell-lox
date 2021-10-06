module PrattParser where

import Control.Monad.State.Strict

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
nud _ = error "only literal supported for nud"

prec :: Token -> Int
prec tok = case tok of
  Number _ -> 0
  EndTok -> 0
  Minus -> 10
  Plus -> 10
  Star -> 20
  Slash -> 20
  Exp -> 30
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
    Exp -> do
      right <- expression $ (prec tok) - 1
      return $ left ** right
    --(Number x) -> return x
    _ -> error $ show tok ++ "not supported"


expression :: Int -> TokenS
expression rbp = do
  token <- nextToken
  left <- nud token
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


expr1 = [(Number 1), Plus, (Number 2), Plus,  (Number 3), Plus, (Number 4), Plus, (Number 5), EndTok]
expr2 = [(Number 10), Minus, (Number 20), Plus, (Number 10), Plus, (Number 20), EndTok]
expr3 = [(Number 10), Star, (Number 20), Star, (Number 10), Slash, (Number 5), EndTok]
expr4 = [(Number 1), Plus, (Number 2), Star, (Number 3), Plus, (Number 10), Slash, (Number 5), EndTok]
expr5 = [(Number 1), Plus, (Number 2), Star, (Number 3), Plus, (Number 10), Slash, Minus, (Number 5), EndTok]
expr6 = [(Number 3), Exp, (Number 2), Exp, (Number 3), EndTok]

evalExpression = map (runState (expression 0)) [expr1, expr2, expr3, expr4, expr5, expr6]
