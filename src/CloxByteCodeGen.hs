{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module CloxByteCodeGen
where
import System.IO
import Data.Text as T
import Data.List as L
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
interpret (Literal t) = lift $ return $ [OpConstant (SValue t)]
interpret (LoxBool t) = lift $ if t then return [OpTrue] else return [OpFalse]
interpret LoxNil = lift $ return [OpNull]
interpret (Paren expr) = interpret expr
interpret (Identifier i) = return [OpGetGlobal i]
interpret (Unary op expr) = do
  value <- interpret expr
  case op of
    UnaryMinus -> lift $ return $ value ++ [OpNegate]
    UnaryBang -> lift $ return $ value ++ [OpNot]
    -- UnaryBang -> case value of
    --   LoxValueNil -> lift . return $ LoxValueBool True
    --   LoxValueBool b -> lift .return $ LoxValueBool (not b)
     -- _ ->  lift . return $ LoxValueBool True
  -- lift . return $ LoxValueDouble 200000.0
    _ -> error $ show op ++ "not supported under Unary Op"


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
    Gt -> return $ left ++ right ++ [OpGt]
    Gte -> return $ left ++ right ++ [OpLt, OpNot]
    Lt -> return $ left ++ right ++ [OpLt]
    Lte -> return $ left ++ right ++ [OpGt, OpNot]
    NotEqual -> return $ left ++ right ++ [OpEqual, OpNot]
    EqualEqual -> return $ left ++ right ++ [OpEqual]
    -- -- special case of Plus
    Plus -> return $ left ++ right ++ [OpAdd]
      -- case (right_expr, left_expr) of
      -- (LoxValueString x, LoxValueString y) -> lift .return $ LoxValueString $ x <> y
      -- (LoxValueDouble x, LoxValueDouble y) -> lift . return $ LoxValueDouble $ x + y
      -- (x, y) -> ExceptT . return . Left $ T.pack $ "Unsupported operation (+) on "  ++ show x ++ " and " ++ show y
interpret (Assignment lhs rhs) = do
  rhs_code <- interpret rhs
  return $ rhs_code ++ [OpSetGlobal lhs]
  -- s <- get
  -- lox_value <- interpret rhs
  -- s' <- get  -- need to get an s after all rhs are processed
  -- case updateEnv lhs lox_value s' of
  --   Just s'' -> do
  --     put $ s''
  --     lift . return  $ lox_value
  --   Nothing -> ExceptT . return . Left $ "Assignment to variable before declaration :" <> lhs

-- interpret (Logical expr1 op expr2) = do
--   result <- interpret expr1
--   case (op, isTruthy result) of
--     (Or, True) -> return result
--     (And, False) -> return result
--     _ -> interpret expr2
interpret x = error $ "not supported " ++ show x

interpretStmt :: Statement -> ByteCodeGenT [OpCode]
interpretStmt (StmtExpr expr) = interpret expr

interpretStmt (StmtPrint expr) = do
  result <- interpret expr
  return $ result ++ [OpPrint]

-- interpretStmt (StmtBlock program) = do
--   s <- get
--   let s' = initEnv (Just s)
--   put s'
--   result <- interpretProgram program
--   s'' <- get
--   case parent s'' of
--     Just p ->  do
--       put p
--       return result
--     Nothing -> do
--       let msg = "Unexpected state of environment where parent is missing from passed in child"
--       liftIO $ print msg
--       ExceptT . return . Left $ msg


-- interpretStmt (StmtIf (IfElse cond ifexpr elseexpr)) = do
--   cond_result <- interpret cond
--   if isTruthy cond_result
--     then interpretStmt ifexpr
--     else maybe (return LoxValueNil) interpretStmt elseexpr

-- interpretStmt (StmtWhile (While cond stmt)) = go
--   where
--     go = do
--       cond_result <- interpret cond
--       if isTruthy cond_result then  do
--         void $ interpretStmt stmt
--         go
--         else return LoxValueSentinel



interpretDeclaration :: Declaration -> ByteCodeGenT [OpCode]
interpretDeclaration (DeclVar (Decl var (Just expr))) = do
  result <- interpret expr
  return $ result ++ [OpDefineGlobal var]

  -- result <- interpret expr
  -- s <- get
  -- let s' = insertEnv var result s
  -- put s'
  -- return LoxValueSentinel
-- interpretDeclaration (DeclVar (Decl var Nothing)) = do
--   s <- get
--   put (insertEnv var LoxValueNil s)
--   return LoxValueSentinel
interpretDeclaration (DeclStatement stmt) = interpretStmt stmt

-- interpretProgram :: Program -> ByteCodeGenT [OpCode]

interpretProgram :: Program -> ByteCodeGenT [OpCode]
interpretProgram decls = do
  opcodes <- mapM interpretDeclaration decls
  return $ L.concat opcodes


compileToByteCode :: T.Text -> IO (Either T.Text [OpCode])
compileToByteCode input = do
  --let y = scanner . T.unpack $ input
  --putStrLn $ show y
  let x = P.parse loxProgram "" $ fromRight [] . scanner . T.unpack $ input
  liftIO $ putStrLn $ show x
  (opcodes, _) <- runStateT (runExceptT (interpretProgram (fromRight [] x))) initEnv
  return opcodes
