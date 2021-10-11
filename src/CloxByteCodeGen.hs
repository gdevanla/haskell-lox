{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module CloxByteCodeGen
where
import System.IO
import Data.Text as T
import Import hiding (many, try, (<|>))

import ExprParser
import Control.Monad.Except
import Data.Map.Strict as M
import Control.Monad.State.Strict
import Data.Maybe
import Scanner (scanner)
import qualified Text.Parsec as P
import qualified Control.Monad
import System.Console.Haskeline

import CloxByteCode


data Env = Env {something::Int}

initEnv :: Env
initEnv = Env {something=1}

type ByteCodeGenT a = ExceptT T.Text (StateT Env IO) a

interpret :: Expr -> ByteCodeGenT [OpCode]
interpret (Number x) = lift $ return [OpConstant (DValue x)]
-- interpret (Literal t) = lift $ return $ LoxValueString t
interpret (LoxBool t) = lift $ return [OpConstant (BValue t)]
interpret LoxNil = lift $ return [OpConstant NullValue]
interpret (Paren expr) = interpret expr
-- interpret (Identifier i) = do
--   s <- get
--   case lookupEnv i s of
--     Just v -> lift . return $ v
--     Nothing -> ExceptT . return . Left $ "Unknown var: " <> i
interpret (Unary op expr) = do
  value <- interpret expr
  case op of
    UnaryMinus -> lift $ return $ value ++ [OpNegate]
    _ -> error $ show op ++ "not supported under Unary Op"
    -- UnaryBang -> case value of
    --   LoxValueNil -> lift . return $ LoxValueBool True
    --   LoxValueBool b -> lift .return $ LoxValueBool (not b)
     -- _ ->  lift . return $ LoxValueBool True
  -- lift . return $ LoxValueDouble 200000.0

interpret (Binary expr1 op expr2) = do
  left <- interpret expr1
  -- right_expr <- unpackIdent right_expr'
  right <- interpret expr2
  -- left_expr <- unpackIdent left_expr'
  case op of
    -- arith operations
    Minus -> return $ left ++ right ++ [OpMinus]
    Star ->  return $ left ++ right ++ [OpStar]
    Slash -> return $ left ++ right ++ [OpSlash]
    -- comparison operations
    -- Gt -> applyCompOpToDouble right_expr left_expr Gt (>)
    -- Gte -> applyCompOpToDouble right_expr left_expr Gt (>=)
    -- Lt -> applyCompOpToDouble right_expr left_expr Gt (<)
    -- Lte -> applyCompOpToDouble right_expr left_expr Gt (<=)
    -- NotEqual -> lift . return $ LoxValueBool $ right_expr /= left_expr
    -- EqualEqual -> lift .return $ LoxValueBool $ right_expr == left_expr
    -- -- special case of Plus
    Plus -> return $ left ++ right ++ [OpAdd]
      -- case (right_expr, left_expr) of
      -- (LoxValueString x, LoxValueString y) -> lift .return $ LoxValueString $ x <> y
      -- (LoxValueDouble x, LoxValueDouble y) -> lift . return $ LoxValueDouble $ x + y
      -- (x, y) -> ExceptT . return . Left $ T.pack $ "Unsupported operation (+) on "  ++ show x ++ " and " ++ show y
-- interpret (Assignment lhs rhs) = do
--   s <- get
--   lox_value <- interpret rhs
--   s' <- get  -- need to get an s after all rhs are processed
--   case updateEnv lhs lox_value s' of
--     Just s'' -> do
--       put $ s''
--       lift . return  $ lox_value
--     Nothing -> ExceptT . return . Left $ "Assignment to variable before declaration :" <> lhs

-- interpret (Logical expr1 op expr2) = do
--   result <- interpret expr1
--   case (op, isTruthy result) of
--     (Or, True) -> return result
--     (And, False) -> return result
--     _ -> interpret expr2

compileToByteCode :: T.Text -> IO (Either T.Text [OpCode])
compileToByteCode input = do
  let x = fromRight LoxNil $ P.parse equality "" $ fromRight [] . scanner . T.unpack $ input
  (opcodes, _) <- runStateT (runExceptT (interpret x)) initEnv
  return opcodes
