{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExprParser where

import Import hiding (many, (<|>), try)
import Data.Text as T
import Data.Char
--import Text.Parsec.String as PS
--import Text.Parsec.Char as PC
import Text.Parsec

import Text.Parsec.Prim
import Text.Parsec.Combinator

import RIO.Partial (read)
import Scanner


-- expression     → equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" ;

-- expression     → literal
--                | unary
--                | binary
--                | grouping ;

-- literal        → NUMBER | STRING | "true" | "false" | "nil" ;
-- grouping       → "(" expression ")" ;
-- unary          → ( "-" | "!" ) expression ;
-- binary         → expression operator expression ;
-- operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
--                | "+"  | "-"  | "*" | "/" ;

type LoxParserResult = Either ParseError Expr

data BinOp = NotEqual | EqualEqual | GT | GTE | LT | LTE | Plus | Minus | Star | Slash
data UnaryOp = UnaryMinus | UnaryBang

data Expr
  = Number LoxTokInfo
  | Literal LoxTokInfo
  | LoxBool Bool
  | LoxNil
  | Paren Expr
  | Unary UnaryOp Expr
  | Binary Expr BinOp Expr


-- satisfy = tokenPrim (t -> String) (SourcePos -> t -> s -> SourcePos) (t -> Maybe a)

-- satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
-- satisfy f           = tokenPrim (\c -> show [c])
--                                 (\pos c _cs -> updatePosChar pos c)
--                                 (\c -> if f c then Just c else Nothing)

type Parser a = ParsecT [LoxTokInfo] () Identity a

satisfyT :: (LoxTokInfo -> Bool) -> Parser LoxTokInfo
satisfyT f = tokenPrim showTok updateTokPos match
  where
    showTok ti = show $ tokinfo_type ti
    updateTokPos _ _ (s:_) = tok_position s
    updateTokPos pos _ [] = pos
    match t = if f t then Just t else Nothing

number :: Parser Expr
number = do
  tokInfo <- satisfyT f
  return $ Number tokInfo
  where
    f (LoxTokInfo (NUMBER _) _ _ _) = True
    f _ = False

literal :: Parser Expr
literal = do
  tokInfo <- satisfyT f
  return $ Literal tokInfo
  where
    f (LoxTokInfo (STRING _) _ _ _) = True
    f _ = False

loxBool :: Parser Expr
loxBool  = do
  tokInfo <- satisfyT f
  return $ f1 tokInfo
  where
    f (LoxTokInfo TRUE _ _ _) = True
    f (LoxTokInfo FALSE _ _ _) = True
    f _ = False

    f1 (LoxTokInfo TRUE _ _ _) = LoxBool True
    f1 (LoxTokInfo FALSE _ _ _) = LoxBool False
    f1 _ = error "Satisy function must be wrong for loxBool"

loxNil :: Parser Expr
loxNil = do
  void $ satisfyT f
  return LoxNil
  where
    f (LoxTokInfo NIL _ _ _) = True
    f _ = False


loxExpr :: Parser Expr
loxExpr = undefined

loxParenExpr :: Parser Expr
loxParenExpr = do
  satisfyT parenOpen *> loxExpr <* satisfyT parenClose
  where
    parenOpen (LoxTokInfo LEFT_PAREN _ _ _) = True
    parenOpen _ = False

    parenClose (LoxTokInfo RIGHT_PAREN _ _ _) = True
    parenClose _ = False

unary :: Parser Expr
unary = Unary <$> (op' <$> satisfyT f) <*> loxExpr
  where
    f (LoxTokInfo BANG _ _ _) = True
    f (LoxTokInfo MINUS _ _ _) = True
    f _ = False

    op' (LoxTokInfo BANG _ _ _) = UnaryBang
    op' (LoxTokInfo MINUS _ _ _) = UnaryMinus
    op' _ = error "satisfy must be wrong for unary op"
