{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module ExprInterpreter(interpret, interpretProgram, runScript, LoxValue(..), lookupEnv, updateEnv, insertEnv, initEnv, runScriptInteractive) where
import System.IO
import Data.Text as T
import Import hiding (many, try, (<|>))

import Data.Time.Clock

import ExprParser
import Control.Monad.Except
import Data.Map.Strict as M
import Control.Monad.State.Strict
import Data.Maybe
import Scanner (scanner)
import qualified Text.Parsec as P
import qualified Control.Monad
import System.Console.Haskeline
import qualified Data.List as L

-- data NativeFunction = Clock |

-- https://www.seas.upenn.edu/~cis552/13fa/lectures/FunEnv.html
data LoxValue
  = LoxValueString T.Text
  | LoxValueDouble Double
  | LoxValueNil
  | LoxValueBool Bool
  | LoxValueIdentifier T.Text
  | LoxValueSentinel -- This is more for the interpreter to return from statements
  | LoxValueFunction T.Text [T.Text] [Declaration]  -- Hold on to the AST
  deriving (Show, Eq)

isTruthy :: LoxValue -> Bool
isTruthy LoxValueNil = False
isTruthy (LoxValueBool x) = x
isTruthy _ = True

-- type Env = M.Map T.Text LoxValue

data Env = Env {
               env :: M.Map T.Text LoxValue,
               parent :: Maybe Env
               } deriving (Show, Eq)

initEnv :: Maybe Env -> Env
initEnv parent = Env {env=M.empty, parent=parent}

lookupEnv :: T.Text -> Env -> Maybe LoxValue
lookupEnv k environ = go (Just environ)
  where
    go (Just Env{..}) = case M.lookup k env of
                           Just v -> Just v
                           Nothing -> go parent
    go Nothing = Nothing

updateEnv :: T.Text -> LoxValue -> Env -> Maybe Env
updateEnv k v s = go (Just s)
  where
    go (Just s'@Env{..}) = case M.lookup k env of
      Just _ -> Just s'{env=M.update (\_ -> Just v) k env}
      Nothing -> do
        parent_state <- go parent
        return $ s'{parent=Just parent_state}
    go Nothing = Nothing

insertEnv :: T.Text -> LoxValue -> Env -> Env
insertEnv k v s@Env {..} = s {env = M.insert k v env}

multiInsertEnv :: [(T.Text, LoxValue)] -> Env -> Env
multiInsertEnv values s@Env {..} = let
  new_env = M.fromList values
  new_env' = M.union new_env env
  in
  s {env=new_env'}


showLoxValue :: LoxValue -> String
showLoxValue (LoxValueString t) = show t
showLoxValue (LoxValueDouble t) = show t
showLoxValue LoxValueNil = "nil"
showLoxValue (LoxValueBool b) = show b
showLoxValue (LoxValueIdentifier b) = show b
showLoxValue LoxValueSentinel = ""

unpackIdent :: LoxValue -> InterpreterTIO
unpackIdent (LoxValueIdentifier x) = do
  s <- get
  let v1 = lookupEnv x s
  case v1 of
    Just v' -> lift . return $ v'
    Nothing -> ExceptT . return $ Left $ "Unknown var: " <> x
unpackIdent x = lift . return $ x

type InterpreterTIO = ExceptT T.Text (StateT Env IO) LoxValue

applyOpToDouble :: LoxValue -> LoxValue -> BinOp -> (Double -> Double -> Double) -> InterpreterTIO
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
applyCompOpToDouble :: LoxValue -> LoxValue -> BinOp -> (Double -> Double -> Bool) -> InterpreterTIO
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



interpret :: Expr -> InterpreterTIO
interpret (Number x) = lift $ return $ LoxValueDouble x
interpret (Literal t) = lift $ return $ LoxValueString t
interpret (LoxBool t) = lift $ return $ LoxValueBool t
interpret LoxNil = lift $ return LoxValueNil
interpret (Paren expr) = interpret expr
interpret (Identifier i) = do
  s <- get
  case lookupEnv i s of
    Just v -> lift . return $ v
    Nothing -> ExceptT . return . Left $ "Unknown var: " <> i
interpret (Unary op expr) = do
  value' <- interpret expr
  value <- unpackIdent value'
  case op of
    UnaryMinus -> case value of
      (LoxValueDouble d) -> lift $ return $ LoxValueDouble (-d)
      d -> ExceptT . return . Left $ T.pack ("Unexpected type: " ++ show d)
    UnaryBang -> case value of
      LoxValueNil -> lift . return $ LoxValueBool True
      LoxValueBool b -> lift .return $ LoxValueBool (not b)
      _ ->  lift . return $ LoxValueBool True
  -- lift . return $ LoxValueDouble 200000.0

interpret (Binary expr1 op expr2) = do
  right_expr' <- interpret expr1
  right_expr <- unpackIdent right_expr'
  left_expr' <- interpret expr2
  left_expr <- unpackIdent left_expr'
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
interpret (Assignment lhs rhs) = do
  s <- get
  lox_value <- interpret rhs
  s' <- get  -- need to get an s after all rhs are processed
  case updateEnv lhs lox_value s' of
    Just s'' -> do
      put $ s''
      lift . return  $ lox_value
    Nothing -> ExceptT . return . Left $ "Assignment to variable before declaration :" <> lhs

interpret (Logical expr1 op expr2) = do
  result <- interpret expr1
  case (op, isTruthy result) of
    (Or, True) -> return result
    (And, False) -> return result
    _ -> interpret expr2

interpret (Call expr arguments _) = do
  callee <- interpret expr
  case callee of
    LoxValueIdentifier callee' -> do
      args <- mapM interpret arguments
      s <- get
      case lookupEnv callee' s of
        Just (LoxValueFunction _ params block) -> do
          let pa = L.zip params args
          s' <- get
          put $ multiInsertEnv pa (initEnv (Just s'))
          interpretProgram block
        Nothing -> ExceptT . return . Left $ "Function not callable"




interpretStmt :: Statement -> InterpreterTIO
interpretStmt (StmtExpr expr) = do
  result <- interpret expr
  modify (insertEnv "_" result)
  return result

interpretStmt (StmtPrint expr) = do
  result <- interpret expr
  liftIO $ putStrLn $ showLoxValue result
  return LoxValueSentinel  -- For now let print return this

interpretStmt (StmtBlock program) = do
  s <- get
  let s' = initEnv (Just s)
  put s'
  result <- interpretProgram program
  s'' <- get
  case parent s'' of
    Just p ->  do
      put p
      return result
    Nothing -> do
      let msg = "Unexpected state of environment where parent is missing from passed in child"
      liftIO $ print msg
      ExceptT . return . Left $ msg


interpretStmt (StmtIf (IfElse cond ifexpr elseexpr)) = do
  cond_result <- interpret cond
  if isTruthy cond_result
    then interpretStmt ifexpr
    else maybe (return LoxValueNil) interpretStmt elseexpr

interpretStmt (StmtWhile (While cond stmt)) = go
  where
    go = do
      cond_result <- interpret cond
      if isTruthy cond_result then  do
        void $ interpretStmt stmt
        go
        else return LoxValueSentinel


-- interpretDeclaration :: Declaration -> Env -> IO (Env, Maybe T.Text)
interpretDeclaration :: Declaration -> InterpreterTIO
interpretDeclaration (DeclVar (Decl var (Just expr))) = do
  result <- interpret expr
  s <- get
  let s' = insertEnv var result s
  put s'
  return LoxValueSentinel

interpretDeclaration (DeclVar (Decl var Nothing)) = do
  s <- get
  put (insertEnv var LoxValueNil s)
  return LoxValueSentinel

interpretDeclaration (DeclStatement stmt) = interpretStmt stmt

interpretDeclaration (DeclFun (Func func_name params block)) = do
  s <- get
  let func = LoxValueFunction func_name params block
  put (insertEnv func_name func s)
  return LoxValueSentinel


interpretProgram :: Program -> InterpreterTIO
interpretProgram (decl : decls) = go
  where
    go = do
      result <- interpretDeclaration decl
      case result of
        LoxValueSentinel -> return ()
        _ -> liftIO $ print result
      interpretProgram decls
interpretProgram [] = return LoxValueSentinel

runScript :: T.Text -> IO ()
runScript script = do
  let lex_result = scanner (T.unpack script)
  case lex_result of
    Right lex -> do
      let ast = P.parse loxProgram "" lex
      case ast of
        Right ast' -> do
          let w = runExceptT (interpretProgram ast')
          (result, _) <- runStateT w (initEnv Nothing)
          print (result , ast')
        Left e -> print $ "Parser error: " <> show e
    Left e -> print $ "Lexer error: " <> show e


type HaskellLineT = InputT (StateT Env IO) ()

runScriptInteractive :: IO ((), Env)
runScriptInteractive = runStateT (runInputT defaultSettings loop) (initEnv Nothing)
  where
    loop :: HaskellLineT
    loop = do
      minput <- getInputLine "%"
      when (isNothing minput) loop
      env <- lift get
      let lex_result = scanner (fromJust minput)
      case lex_result of
        Right lex -> do
          let ast = P.parse loxProgram "" lex
          case ast of
            Right ast' -> do
              let w = runExceptT (interpretProgram ast')
              (result, env') <- liftIO $ runStateT w env
              -- liftIO $ print result
              lift $ put env'
              loop
            Left e -> do
              liftIO $ putStrLn $ "Parser error: " <> show e
              loop
        Left e -> do
          liftIO $ putStrLn $ "Lexer error: " <> show e
          loop
