{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExprParser where

import Data.Char
import Data.Text as T
import Import hiding (many, try, (<|>))
--import Text.Parsec.String as PS
--import Text.Parsec.Char as PC

import RIO.Partial (read)
import Scanner
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim

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

data BinOp = NotEqual | EqualEqual | Gt | Gte | Lt | Lte | Plus | Minus | Star | Slash deriving (Show, Eq)

data UnaryOp = UnaryMinus | UnaryBang deriving (Show, Eq)

data Expr
  = Number Double   -- LoxTokInfo
  | Literal T.Text -- LoxTokInfo
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

satisfyT :: (LoxTokInfo -> Bool) -> Parser LoxTokInfo
satisfyT f = tokenPrim showTok updateTokPos match
  where
    showTok ti = show $ tokinfo_type ti
    updateTokPos _ _ (s : _) = tok_position s
    updateTokPos pos _ [] = pos
    match t = if f t then Just t else Nothing

number :: Parser Expr
number = do
  tokInfo <- satisfyT f
  return $ Number (value tokInfo)
  where
    f (LoxTokInfo (NUMBER _) _ _ _) = True
    f _ = False

    value (LoxTokInfo (NUMBER x) _ _ _) = x
    value _ = error "Bad Satisfy"

literal :: Parser Expr
literal = do
  tokInfo <- satisfyT f
  return $ Literal (value tokInfo)
  where
    f (LoxTokInfo (STRING _) _ _ _) = True
    f _ = False

    value (LoxTokInfo (STRING x) _ _ _) = T.pack x
    value _ = error "Bad Satisfy"

loxBool :: Parser Expr
loxBool = do
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

loxParenExpr :: Parser Expr
loxParenExpr = do
  satisfyT parenOpen *> loxExpr <* satisfyT parenClose
  where
    parenOpen (LoxTokInfo LEFT_PAREN _ _ _) = True
    parenOpen _ = False

    parenClose (LoxTokInfo RIGHT_PAREN _ _ _) = True
    parenClose _ = False

loxPrimary :: Parser Expr
loxPrimary = number <|> literal <|> loxBool <|> loxNil <|> loxParenExpr

unary' :: Parser Expr
unary' = Unary <$> (op' <$> satisfyT f) <*> unary
  where
    f (LoxTokInfo BANG _ _ _) = True
    f (LoxTokInfo MINUS _ _ _) = True
    f _ = False

    op' (LoxTokInfo BANG _ _ _) = UnaryBang
    op' (LoxTokInfo MINUS _ _ _) = UnaryMinus
    op' _ = error "satisfy must be wrong for unary op"

unary :: Parser Expr
unary = unary' <|> loxPrimary

factor :: Parser Expr
factor = do
  expr <- unary
  try (secondPart expr) <|> return expr
  where
    secondPart expr1 = Binary expr1 <$> (op' <$> satisfyT f) <*> unary
    f x = case tokinfo_type x of
      x' | x' `elem` [STAR, SLASH] -> True
         | otherwise -> False

    op' (LoxTokInfo SLASH _ _ _) = Slash
    op' (LoxTokInfo STAR _ _ _) = Star
    op' _ = error "satisfy must be wrong for unary op"


term :: Parser Expr
term = do
  expr <- factor
  try (secondPart expr) <|> return expr
  where
    secondPart expr1 = Binary expr1 <$> (op' <$> satisfyT f) <*> factor
    f x = case tokinfo_type x of
      x' | x' `elem` [MINUS, PLUS] -> True
         | otherwise -> False

    op' (LoxTokInfo MINUS _ _ _) = Minus
    op' (LoxTokInfo PLUS _ _ _) = Plus
    op' _ = error "satisfy must be wrong for unary op"


comparison :: Parser Expr
comparison = do
  expr <- term
  try (secondPart expr) <|> return expr
  where
    secondPart expr1 = Binary expr1 <$> (op' <$> satisfyT f) <*> comparison
    f x = case tokinfo_type x of
      x' | x' `elem` [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] -> True
         | otherwise -> False

    op' (LoxTokInfo GREATER _ _ _) = Gt
    op' (LoxTokInfo GREATER_EQUAL _ _ _) = Gte
    op' (LoxTokInfo LESS _ _ _) = Lt
    op' (LoxTokInfo LESS_EQUAL _ _ _) = Lte
    op' _ = error "satisfy must be wrong for unary op"


equality :: Parser Expr
equality = do
  expr <- comparison
  try (secondPart expr) <|> return expr
  where
    secondPart expr1 = Binary expr1 <$> (op' <$> satisfyT f) <*> comparison
    f x = case tokinfo_type x of
      x' | x' `elem` [BANG_EQUAL , EQUAL_EQUAL] -> True
         | otherwise -> False

    op' (LoxTokInfo BANG_EQUAL _ _ _) = NotEqual
    op' (LoxTokInfo EQUAL _ _ _) = EqualEqual
    op' _ = error "satisfy must be wrong for unary op"


loxExpr :: Parser Expr
loxExpr = equality

scannerLoxTokens :: [LoxTokInfo] -> LoxParserResult
scannerLoxTokens = parse loxExpr ""

-- binary :: Parser Expr
-- binary = do
--   l <- loxExpr
--   op <- satisfyT (f . tokinfo_type)
--   r <- loxExpr
--   return $ Binary l (op' (tokinfo_type op)) r
--   where
--     f x
--       | x `elem` [BANG_EQUAL, EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, PLUS, MINUS, STAR, SLASH] = True
--       | otherwise = False

--     op' BANG_EQUAL = NotEqual
--     op' EQUAL = EqualEqual
--     op' GREATER = Gt
--     op' GREATER_EQUAL = Gte
--     op' LESS = Lt
--     op' LESS_EQUAL = Lte
--     op' PLUS = Plus
--     op' MINUS = Minus
--     op' STAR = Star
--     op' SLASH = Slash
--     op' _ = error "Satisfy probably wrong"
