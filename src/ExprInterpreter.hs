{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExprInterpreter where
import System.IO
import Data.Text as T
import Import hiding (many, try, (<|>))
import Scanner
import Text.Parsec

import ExprParser

-- https://www.seas.upenn.edu/~cis552/13fa/lectures/FunEnv.html
data LoxValue
  = LoxValueString T.Text
  | LoxValueDouble Double
  | LoxValueNil
  | LoxValueBool Bool
  | LoxValueIdentifier T.Text
  deriving (Show, Eq)

showLoxValue :: LoxValue -> String
showLoxValue (LoxValueString t) = show t
showLoxValue (LoxValueDouble t) = show t
showLoxValue LoxValueNil = "nil"
showLoxValue (LoxValueBool b) = show b
showLoxValue (LoxValueIdentifier b) = show b

applyOpToDouble :: LoxValue -> LoxValue -> BinOp -> (Double -> Double -> Double) -> Either T.Text LoxValue
applyOpToDouble (LoxValueDouble x) (LoxValueDouble y) bop op = Right $ LoxValueDouble $ op x y
applyOpToDouble x y bop _ = Left value
  where
    value =
      T.pack $
        "Unsupported operation "
          ++ show bop
          ++ " between "
          ++ show x
          ++ " and "
          ++ show y

applyCompOpToDouble :: LoxValue -> LoxValue -> BinOp -> (Double -> Double -> Bool) -> Either T.Text LoxValue
applyCompOpToDouble (LoxValueDouble x) (LoxValueDouble y) bop op = Right $ LoxValueBool $ op x y
applyCompOpToDouble x y bop _ = Left value
  where
    value =
      T.pack $
        "Unsupported operation "
          ++ show bop
          ++ " between "
          ++ show x
          ++ " and "
          ++ show y


interpret :: Expr -> Either T.Text LoxValue
interpret (Number x) = Right $ LoxValueDouble x
interpret (Literal t) = Right $ LoxValueString t
interpret (LoxBool t) = Right $ LoxValueBool t
interpret LoxNil    = Right LoxValueNil
interpret (Paren expr) = interpret expr
interpret (Identifier i) = Right $ LoxValueIdentifier i
interpret (Unary op expr) = let
  value = interpret expr
  result = case op of
    UnaryMinus -> case value of
      Right (LoxValueDouble d) -> Right $ LoxValueDouble (-d)
      Right d -> Left (T.pack ("Unexpected type" ++ show d))
      x -> x
    UnaryBang -> case value of
      Right LoxValueNil -> Right $ LoxValueBool True
      Right (LoxValueBool b) -> Right $ LoxValueBool (not b)
      Right _ -> Right $ LoxValueBool True
      _ -> value
  in
  result
interpret (Binary expr1 op expr2) = do
  right_expr <- interpret expr1
  left_expr <- interpret expr2
  case op of
    -- arith operations
    Minus -> applyOpToDouble right_expr left_expr Minus (-)
    Star -> applyOpToDouble right_expr left_expr Star (*)
    Slash -> applyOpToDouble right_expr left_expr Star (/)
    -- comparison operations
    Gt -> applyCompOpToDouble right_expr left_expr Gt (>)
    Gte -> applyCompOpToDouble right_expr left_expr Gt (>=)
    Lt -> applyCompOpToDouble right_expr left_expr Gt (<)
    Lte -> applyCompOpToDouble right_expr left_expr Gt (<=)
    NotEqual -> Right $ LoxValueBool $ right_expr /= left_expr
    EqualEqual -> Right $ LoxValueBool $ right_expr == left_expr
    -- special case of Plus
    Plus -> case (right_expr, left_expr) of
      (LoxValueString x, LoxValueString y) -> Right $ LoxValueString $ x <> y
      (LoxValueDouble x, LoxValueDouble y) -> Right $ LoxValueDouble $ x + y
      (x, y) -> Left $ T.pack $ "Unsupported operation (+) on "  ++ show x ++ " and " ++ show y

interpretStmt :: Statement -> IO ()
interpretStmt (StmtExpr expr) = return ()  -- will be filled in when we maintain state
interpretStmt (StmtPrint expr) = do
  case interpret expr of
    Right x -> putStrLn $ showLoxValue x
    Left x -> print $ "Unexpected error" <> x

interpretProgram :: [Statement] -> IO ()
interpretProgram = mapM_ interpretStmt
