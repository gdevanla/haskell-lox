{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExprInterpreter where
import System.IO
import Data.Text as T
import Import hiding (many, try, (<|>))
import Scanner
import Text.Parsec hiding (State)

import ExprParser
import Control.Monad.Except
import Data.Map as M
import Control.Monad.State.Strict

-- https://www.seas.upenn.edu/~cis552/13fa/lectures/FunEnv.html
data LoxValue
  = LoxValueString T.Text
  | LoxValueDouble Double
  | LoxValueNil
  | LoxValueBool Bool
  | LoxValueIdentifier T.Text
  deriving (Show, Eq)

type Env = M.Map T.Text LoxValue
type InterpreterT = ExceptT T.Text (State Env) LoxValue

showLoxValue :: LoxValue -> String
showLoxValue (LoxValueString t) = show t
showLoxValue (LoxValueDouble t) = show t
showLoxValue LoxValueNil = "nil"
showLoxValue (LoxValueBool b) = show b
showLoxValue (LoxValueIdentifier b) = show b

applyOpToDouble :: LoxValue -> LoxValue -> BinOp -> (Double -> Double -> Double) -> InterpreterT
applyOpToDouble (LoxValueDouble x) (LoxValueDouble y) bop op = lift . return $ LoxValueDouble $ op x y
applyOpToDouble x y bop _ = ExceptT . return $ Left value
  where
    value =
      T.pack $
        "Unsupported operation "
          ++ show bop
          ++ " between "
          ++ show x
          ++ " and "
          ++ show y

applyCompOpToDouble :: LoxValue -> LoxValue -> BinOp -> (Double -> Double -> Bool) -> InterpreterT
applyCompOpToDouble (LoxValueDouble x) (LoxValueDouble y) bop op = lift . return $ LoxValueBool $ op x y
applyCompOpToDouble x y bop _ = ExceptT . return $ Left value
  where
    value =
      T.pack $
        "Unsupported operation "
          ++ show bop
          ++ " between "
          ++ show x
          ++ " and "
          ++ show y


interpret :: Expr -> InterpreterT
interpret (Number x) = lift $ return $ LoxValueDouble x
interpret (Literal t) = lift $ return $ LoxValueString t
interpret (LoxBool t) = lift $ return $ LoxValueBool t
interpret LoxNil    = lift $ return LoxValueNil
interpret (Paren expr) = interpret expr
interpret (Identifier i) = do
  s <- get
  case M.lookup i s of
    Just v -> lift . return $ v
    Nothing -> ExceptT . return . Left $ "Unknown var: " <> i
interpret (Unary op expr) = do
  value <- interpret expr
  case op of
    UnaryMinus -> case value of
      (LoxValueDouble d) -> lift $ return $ LoxValueDouble (-d)
      d -> ExceptT . return . Left $ T.pack ("Unexpected type" ++ show d)
    UnaryBang -> case value of
      LoxValueNil -> lift . return $ LoxValueBool True
      LoxValueBool b -> lift .return $ LoxValueBool (not b)
      _ ->  lift . return $ LoxValueBool True
  lift . return $ value

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
    NotEqual -> lift . return $ LoxValueBool $ right_expr /= left_expr
    EqualEqual -> lift .return $ LoxValueBool $ right_expr == left_expr
    -- special case of Plus
    Plus -> case (right_expr, left_expr) of
      (LoxValueString x, LoxValueString y) -> lift .return $ LoxValueString $ x <> y
      (LoxValueDouble x, LoxValueDouble y) -> lift . return $ LoxValueDouble $ x + y
      (x, y) -> ExceptT . return . Left $ T.pack $ "Unsupported operation (+) on "  ++ show x ++ " and " ++ show y

interpretStmt :: Statement -> Env -> IO Env
interpretStmt (StmtExpr expr) s = do
  let (result, s') = runState (runExceptT (interpret expr)) s
  case result of
    Right _ -> return s'
    Left e -> do
      print e
      return s'

interpretStmt (StmtPrint expr) s = do
  let (result, s') = runState (runExceptT (interpret expr)) s
  case result of
    Right x -> putStrLn $ showLoxValue x
    Left x -> print $ "Unexpected error" <> x
  return s'

interpretDeclaration :: Declaration -> Env -> IO Env
interpretDeclaration (DeclVar (Decl var (Just expr))) s = do
  let (result, s') = runState (runExceptT (interpret expr)) s
  case result of
        Right r -> do
          print $ "setting value of " <> var <> " to " <> T.pack (show r)
          return $ M.insert var r s'
        _ -> do
          print $ "Error during declaration of" <> var
          return s'

interpretDeclaration (DeclVar (Decl var Nothing)) s = do
  return $ M.insert var LoxValueNil s

interpretDeclaration (DeclStatement stmt) s = interpretStmt stmt s

interpretProgram :: Program -> Env -> IO ()
interpretProgram (decl : decls) s = go
  where
    go  = do
      s' <- interpretDeclaration decl s
      print $ show s'
      interpretProgram decls s'
      return ()
interpretProgram [] _ = return ()
