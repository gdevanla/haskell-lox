{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExprParser where

import Data.Text as T
import Import hiding (many, try, (<|>))
import Scanner
import Text.Parsec

-- https://craftinginterpreters.com/parsing-expressions.html
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

data BinOp = NotEqual | EqualEqual | Gt | Gte | Lt | Lte | Plus | Minus | Star | Slash
  deriving (Show, Eq)

data UnaryOp = UnaryMinus | UnaryBang deriving (Show, Eq)

data Expr
  = Number Double
  | Literal T.Text
  | LoxBool Bool
  | LoxNil
  | Paren Expr
  | Unary UnaryOp Expr
  | Binary Expr BinOp Expr
  deriving (Show, Eq)

-- satisfy = tokenPrim (t -> String) (SourcePos -> t -> s -> SourcePos) (t -> Maybe a)

-- satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
-- satisfy f           = tokenPrim (\c -> show [c])
--                                 (\pos c _cs -> updatePosChar pos c)
--                                 (\c -> if f c then Just c else Nothing)

type Parser a = ParsecT [LoxTokInfo] () Identity a

satisfyT :: (LoxTokInfo -> Maybe a) -> Parser a
satisfyT f = tokenPrim showTok updateTokPos match
  where
    showTok ti = show $ tokinfo_type ti
    updateTokPos _ _ (s : _) = tok_position s
    updateTokPos pos _ [] = pos
    match t = f t

-- this is similar to chainl in `Text.Parsec` but works on `BinOp`
-- adopted from https://jakewheat.github.io/intro_to_parsing/
leftChain :: Parser Expr -> Parser BinOp -> Parser Expr
leftChain p op = do
  expr <- p
  maybeAddSuffix expr
  where
    addSuffix e0 = do
      op' <- op
      e1 <- p
      maybeAddSuffix (Binary e0 op' e1)

    maybeAddSuffix e = addSuffix e <|> return e


-- primary
number :: Parser Expr
number = satisfyT f
  where
    f (LoxTokInfo (NUMBER x) _ _ _) = Just (Number x)
    f _ = Nothing

literal :: Parser Expr
literal = satisfyT f
  where
    f (LoxTokInfo (STRING x) _ _ _) = Just (Literal $ T.pack x)
    f _ = Nothing


loxBool :: Parser Expr
loxBool = satisfyT f
  where
    f (LoxTokInfo TRUE _ _ _) = Just (LoxBool True)
    f (LoxTokInfo FALSE _ _ _) = Just (LoxBool False)
    f _ = Nothing


loxNil :: Parser Expr
loxNil = satisfyT f
  where
    f (LoxTokInfo NIL _ _ _) = Just LoxNil
    f _ = Nothing

loxParenExpr :: Parser Expr
loxParenExpr = do
  satisfyT parenOpen *> loxExpr <* satisfyT parenClose
  where
    -- use LoxNil as placeholder, since we do not have an equilivalent Expr for Paren
    parenOpen (LoxTokInfo LEFT_PAREN _ _ _) = Just ()
    parenOpen _ = Nothing

    parenClose (LoxTokInfo RIGHT_PAREN _ _ _) = Just ()
    parenClose _ = Nothing

loxPrimary :: Parser Expr
loxPrimary = number <|> literal <|> loxBool <|> loxNil <|> loxParenExpr

unary' :: Parser Expr
unary' = Unary <$> satisfyT f <*> unary
  where
    f (LoxTokInfo BANG _ _ _) = Just UnaryBang
    f (LoxTokInfo MINUS _ _ _) = Just UnaryMinus
    f _ = Nothing


unary :: Parser Expr
unary = unary' <|> loxPrimary

factor :: Parser Expr
factor = leftChain unary (satisfyT f)
  where
    f x = case tokinfo_type x of
      STAR -> Just Star
      SLASH -> Just Slash
      _ -> Nothing

term :: Parser Expr
term = leftChain factor (satisfyT f)
  where
    f x = case tokinfo_type x of
      MINUS -> Just Minus
      PLUS -> Just Plus
      _ -> Nothing


comparison :: Parser Expr
comparison = leftChain term (satisfyT f)
  where
    f x = case tokinfo_type x of
      GREATER -> Just Gt
      GREATER_EQUAL -> Just Gte
      LESS -> Just Lt
      LESS_EQUAL -> Just Lte
      _ -> Nothing

equality :: Parser Expr
equality = leftChain comparison (satisfyT f)
  where
    f x = case tokinfo_type x of
      BANG_EQUAL -> Just NotEqual
      EQUAL_EQUAL -> Just EqualEqual
      _ -> Nothing


loxExpr :: Parser Expr
loxExpr = equality

scannerLoxTokens :: [LoxTokInfo] -> LoxParserResult
scannerLoxTokens = parse loxExpr ""