module CloxCompiler where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Either
import Data.List as L
import Data.Sequence as Seq
import Data.Text as T
import System.IO (putStr)
import qualified Text.Parsec as PS
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Pos
import CloxByteCode

import Import
import Text.Parsec

import Scanner

-- type TokenS = State [LoxTokInfo] [OpCode]

type CloxParser a = ParsecT [LoxTokInfo] () Identity a

nextToken :: CloxParser LoxTokInfo
nextToken = do
  s <- getParserState
  case stateInput s of
    (x:xs) -> do
      void $ updateParserState (\s'-> s'{stateInput=xs})
      return x
    -- _ -> return $ LoxTokInfo EOF Nothing Nothing (newPos "" 0 0)
    _ -> error "Token list is empty in nextToken"

currToken :: CloxParser LoxTokInfo
currToken = do
  s <- getParserState
  case stateInput s of
    (x:_) -> return x
    _ ->  error "No more tokens"

nud :: LoxTokInfo -> CloxParser [OpCode]
nud (LoxTokInfo (NUMBER !x) _ _ _) = return [OpConstant (DValue x)]
nud tok@(LoxTokInfo MINUS _ _ _) = do
  right <- expression (prefixPrec tok)
  return $ right ++ [OpNegate]
nud (LoxTokInfo LEFT_PAREN _ _ _) = do
  right <- expression 0
  token <- currToken
  case token of
    (LoxTokInfo RIGHT_PAREN _ _ _) -> do
      void nextToken
      return right
    _ -> error $ "unexpected token = " ++ show token ++ " found."
nud t = error $ "only literal supported for nud, not support for " ++ show t

prec :: LoxTokInfo -> Double
prec tok = case tokinfo_type tok of
  NUMBER _ -> 0
  LEFT_PAREN -> 0
  RIGHT_PAREN -> 0
  EOF -> 0
  SEMICOLON -> 0
  MINUS -> 10
  PLUS -> 10
  STAR -> 20
  SLASH -> 20
  -- EXP -> 30

prefixPrec :: LoxTokInfo -> Double
prefixPrec tok = case tokinfo_type tok of
  NUMBER _ -> 0
  MINUS -> 10
  _ -> error $ "prefix_prec not defined for = " ++ show tok

led :: [OpCode] -> LoxTokInfo -> CloxParser [OpCode]
led left tok = do
  case tokinfo_type tok of
    PLUS -> do
      right <- expression (prec tok)
      return $ left ++ right ++ [OpAdd]
    MINUS -> do
      right <- expression (prec tok)
      return $ right ++ left ++ [OpMinus]
    STAR -> do
      right <- expression (prec tok)
      return $ left ++ right ++ [OpStar]
    SLASH -> do
      right <- expression (prec tok)
      return $ right ++ left ++ [OpSlash]
    _ -> error $ show tok ++ "not supported"


expression :: Double -> CloxParser [OpCode]
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


compileToByteCode :: T.Text -> Either ParseError [OpCode]
compileToByteCode = parse (expression 0) "" . (fromRight [] . scanner . T.unpack)

evalExpression :: String -> Chunk
evalExpression expr = let
  a = parse (expression 0) ""  . (fromRight [] . scanner) $ expr
  in
  case a of
    Right z -> Chunk $ Seq.fromList z
    Left e -> error $ "Not expecting an error" ++ show e

evalExpressions :: [Chunk]
evalExpressions = let
  expressions =
    [ "1+2+3+4;",
      "10-2+1;",
      "10-5-1;",
      "10+2*3-8;",
      --"3^2^3;",
      "(10+2)*3-8;"
    ]
   in
    L.map evalExpression expressions

evalAndPrint:: IO ()
evalAndPrint = do
  let results = L.zipWith disassembleChunk evalExpressions (L.map (T.pack . show)([1..]::[Int]))
  mapM_ (System.IO.putStr . T.unpack . uncurry (<>)) results
