{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module ExprInterpreter(interpret, interpretProgram, runScript, LoxValue(..), lookupEnv, updateEnv, insertEnv, initEnv, runScriptInteractive) where
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

-- https://www.seas.upenn.edu/~cis552/13fa/lectures/FunEnv.html
data LoxValue
  = LoxValueString T.Text
  | LoxValueDouble Double
  | LoxValueNil
  | LoxValueBool Bool
  | LoxValueIdentifier T.Text
  deriving (Show, Eq)

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

type InterpreterT = ExceptT T.Text (State Env) LoxValue

showLoxValue :: LoxValue -> String
showLoxValue (LoxValueString t) = show t
showLoxValue (LoxValueDouble t) = show t
showLoxValue LoxValueNil = "nil"
showLoxValue (LoxValueBool b) = show b
showLoxValue (LoxValueIdentifier b) = show b

unpackIdent :: LoxValue -> InterpreterT
unpackIdent (LoxValueIdentifier x) = do
  s <- get
  let v1 = lookupEnv x s
  case v1 of
    Just v' -> lift . return $ v'
    Nothing -> ExceptT . return $ Left $ "Unknown var" <> x
unpackIdent x = lift . return $ x

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
  case lookupEnv i s of
    Just v -> lift . return $ v
    Nothing -> ExceptT . return . Left $ "Unknown var: " <> i
interpret (Unary op expr) = do
  value' <- interpret expr
  value <- unpackIdent value'
  case op of
    UnaryMinus -> case value of
      (LoxValueDouble d) -> lift $ return $ LoxValueDouble (-d)
      d -> ExceptT . return . Left $ T.pack ("Unexpected type" ++ show d)
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
    Nothing -> ExceptT . return . Left $ "Assignment to variable before declaration " <> lhs


interpretStmt :: Statement -> Env -> IO (Env, Maybe T.Text)
interpretStmt (StmtExpr expr) s = do
  let (result, s') = runState (runExceptT (interpret expr)) s
  case result of
    Right _ -> return (s', Nothing)
    Left e -> do
      print e
      return (s', Just e)

interpretStmt (StmtPrint expr) s = do
  let (result, s') = runState (runExceptT (interpret expr)) s
  case result of
    Right x -> do
      putStrLn $ showLoxValue x
      return (s', Nothing)
    Left x -> do
      let msg = "Unexpected error" <> x
      print msg
      return (s', Just msg)

interpretStmt (StmtBlock program) s = do
  let s' = initEnv (Just s)
  (s'', msg) <- interpretProgram program s'
  case parent s'' of
    Just p ->  return (p, msg)
    Nothing -> return (s', Just "Unexpected state of environment where parent is missing from passed in child")

interpretStmt (StmtIf (IfElse cond ifexpr elseexpr)) s = do
  let (cond_result, s') = runState (runExceptT (interpret cond)) s
  case cond_result of
    Right cr -> do
      if is_truthy cr
        then interpretStmt ifexpr s'
        else if isJust elseexpr then interpretStmt (fromJust elseexpr) s' else return (s, Nothing)
    Left cr -> do
      let msg = "Unexpected error" <> cr
      print msg
      return (s, Just msg)
  where
    is_truthy LoxValueNil = False
    is_truthy (LoxValueBool x) = x
    is_truthy _ = True


interpretDeclaration :: Declaration -> Env -> IO (Env, Maybe T.Text)
interpretDeclaration (DeclVar (Decl var (Just expr))) s = do
  let (result, s') = runState (runExceptT (interpret expr)) s
  case result of
        Right r -> do
          -- print $ "setting value of " <> var <> " to " <> T.pack (show r)
          return $ (insertEnv var r s', Nothing)
        _ -> do
          let msg = "Error during declaration of" <> var
          -- print msg
          return (s', Just msg)

interpretDeclaration (DeclVar (Decl var Nothing)) s = do
  return (insertEnv var LoxValueNil s, Nothing)

interpretDeclaration (DeclStatement stmt) s = interpretStmt stmt s



interpretProgram :: Program -> Env -> IO (Env, Maybe T.Text)
interpretProgram (decl : decls) s = go
  where
    go = do
      (s', msg) <- interpretDeclaration decl s
      case msg of
        Just msg' -> return (s', Just msg')
        Nothing -> do
          (s'', msg'') <- interpretProgram decls s'
          return (s'', msg'')
interpretProgram [] env = return (env, Nothing)

runScript :: T.Text -> IO ()
runScript script = do
  let lex_result = scanner (T.unpack script)
  case lex_result of
    Right lex -> do
      let ast = P.parse loxProgram "" lex
      case ast of
        Right ast' -> do
          (_, msg) <- interpretProgram ast' (initEnv Nothing)
          when (isJust msg) $ print msg
        Left e -> print $ "Scanner error" <> show e
    Left e -> print $ "Lexer error" <> show e


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
              (env', msg) <- liftIO $ interpretProgram ast' env
              when (isJust msg) $ liftIO $ print msg
              lift $ put env'
              loop
            Left e -> do
              liftIO $ print $ "Scanner error" <> show e
              loop
        Left e -> do
          liftIO $ print $ "Lexer error" <> show e
          loop

-- runScriptInteractive = runInputT defaultSettings loop
--   where
--     loop :: InputT IO ()
--     loop = do
--       minput <- getInputLine "% "
--       case minput of
--         Nothing -> return ()
--         Just "quit" -> return ()
--         Just input -> do outputStrLn $ "Input was: " ++ input
--                          loop


-- runScriptInteractive :: IO ()
-- runScriptInteractive = do
--   System.IO.hSetEcho stdin False
--   putStrLn "Starting lox interactive prompt"
--   go (initEnv Nothing)
--   where
--     go env = do
--       putStr "lox> "
--       line <- getLine
--       print line
--       when (line == "") (go env)
--       let lex_result = scanner line
--       case lex_result of
--         Right lex -> do
--           let ast = P.parse loxProgram "" lex
--           case ast of
--             Right ast' -> do
--               (env', msg) <- interpretProgram ast' env
--               when (isJust msg) $ print msg
--               go env'
--             Left e -> do
--               print $ "Scanner error" <> show e
--               go env
--         Left e -> print $ "Lexer error" <> show e
