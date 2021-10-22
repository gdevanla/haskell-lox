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
import Data.Sequence as Seq

import CloxByteCode


data Env = Env {
  chunk_type:: !ChunkType,
  local_count:: !Int,
  scope_depth:: !Int,
  locals:: ![Local]
  } deriving (Show, Eq)

data ChunkType = ChunkScript | ChunkFunction
  deriving (Show, Eq)

data Local = Local !T.Text {-#UNPACK#-} !Int deriving (Show, Eq)

initEnv :: Env
initEnv = Env {local_count = 0, scope_depth = 0, locals = [Local "" 0], chunk_type = ChunkScript}

initEnvFunc :: T.Text -> [T.Text] -> Int -> Env
initEnvFunc funcname params scope = let
  all_locals = L.reverse $ Local funcname scope:[Local x scope |  x<-params]
  in
  Env {local_count = 0, scope_depth = scope, locals = all_locals, chunk_type = ChunkFunction}

updateScopeDepth :: (Int->Int->Int) -> ByteCodeGenT ()
updateScopeDepth op = do
  env <- get
  put $ env {scope_depth = op (scope_depth env) 1}

incrScopeDepth :: ByteCodeGenT ()
incrScopeDepth = updateScopeDepth (+)

decrScopeDepth :: ByteCodeGenT ()
decrScopeDepth = updateScopeDepth (-)

getLocalIndex :: T.Text -> ByteCodeGenT (Maybe Int)
getLocalIndex key = do
  env <- get
  let l' = L.reverse $ locals env
  return $ go key l' 0
  where
    go key' (Local n _:xs) offset = if n == key' then Just offset else go key' xs (offset + 1)
    go key' [] _ = Nothing

type ByteCodeGenT a = ExceptT T.Text (StateT Env IO) a

interpret :: Expr -> ByteCodeGenT [OpCode]
interpret (Number x) = lift $ return [OpConstant (DValue x)]
interpret (Literal t) = lift $ return $ [OpConstant (SValue t)]
interpret (LoxBool t) = lift $ if t then return [OpTrue] else return [OpFalse]
interpret LoxNil = lift $ return [OpNull]
interpret (Paren expr) = interpret expr
interpret (Identifier i) = do
  env <- get
  let scope = scope_depth env
  local_index <- getLocalIndex i
  case local_index of
    Just v -> return [OpGetLocal v]
    Nothing -> return [OpGetGlobal i]
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
  env <- get
  local_index <- getLocalIndex lhs
  case local_index of
    Just offset -> return $ rhs_code ++ [OpSetLocal offset]
    Nothing -> return $ rhs_code ++ [OpSetGlobal lhs]
interpret (Logical expr1 op expr2) = do
  case op of
    And -> do
      expr1_opcode <- interpret expr1
      expr2_opcode <- interpret expr2
      let if_jump = OpJumpIfFalse $ L.length expr2_opcode
      return $ expr1_opcode ++ if_jump:expr2_opcode
    Or -> do
      expr1_opcode <- interpret expr1
      expr2_opcode <- interpret expr2
      let if_jump = OpJumpIfFalse 1
      let op_jump = OpJump $ 1 + L.length expr2_opcode
      return $ expr1_opcode ++ [if_jump, op_jump, OpPop] ++ expr2_opcode

interpret (Call !expr !arguments _) = do
  func <- interpret expr
  args <- mapM interpret arguments
  let args' = L.concat args
  liftIO $ print "Setting up call........''"
  liftIO $ print args'
  return $ func ++ args' ++ [OpCall (L.length args)]
  -- case func of
  --   Function fo@FuncObj{..} -> undefined
  --   _ -> ExceptT . return . Left $! "Error calling" ++ show func_name
  -- !callee <- interpret expr
  -- !args <- mapM interpret arguments
  -- !orig <- get
  -- case callee of
  --   LoxValueFunction !func_name !params !block !closure -> do
  --     let !pa = L.zip params args
  --     --liftIO $ putStrLn $ show s'
  --     let !s = multiInsertEnv pa (initEnv (Just closure))
  --     -- we need to insert this back here to resolve circular dependency
  --     let !s' = insertEnv func_name (LoxValueFunction func_name params block closure) s
  --     put $! s'
  --     !value <- catchError (interpretProgram block) f
  --     put orig
  --     return $! value

  -- where
  --   f (SystemError e) = ExceptT . return . Left $ SystemError e
  --   f (ControlFlow (LoxValueReturn e)) = return e
  --   f (ControlFlow v) = ExceptT . return . Left $ SystemError $ "Unknown handler:" <> T.pack (show v)
--{-# INLINE interpret #-}


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

interpretStmt (StmtBlock program) = do
  --void declareBlockName
  interpretAsBlock program
  where
    -- need to simulare function name
    declareBlockName :: ByteCodeGenT ()
    declareBlockName = do
      env <- get
      let scope = scope_depth env
      put $ env {locals = Local "block" scope : locals env}

interpretStmt (StmtReturn (Just expr)) = do
  opcodes <- interpret expr
  return $ opcodes ++ [OpReturn]
interpretStmt (StmtReturn Nothing) = return [OpConstant NullValue, OpReturn]


interpretStmt (StmtIf (IfElse cond ifexpr elseexpr)) = do
  cond_result <- interpret cond
  if_opcodes <- interpretStmt ifexpr
  else_opcodes <- maybe (return []) interpretStmt elseexpr
  let if_jump = OpJumpIfFalse $ L.length if_opcodes + (if L.null else_opcodes then 1 else 2)
  let else_jump = [OpJump $ L.length else_opcodes + 1| not $ L.null else_opcodes]
  let else_pop = [OpPop | not $ L.null else_opcodes]
  return $ cond_result ++ [if_jump, OpPop] ++ if_opcodes ++ else_jump ++ else_pop ++ else_opcodes

interpretStmt (StmtWhile (While cond stmt)) = do
  cond_result <- interpret cond
  stmt_codes <- interpretStmt stmt
  let if_jump = OpJumpIfFalse $ L.length stmt_codes + 3
  let start_of_chunk = 2 + L.length stmt_codes + L.length cond_result + 1
  return $ cond_result ++ if_jump:OpPop:stmt_codes ++ [OpLoopStart start_of_chunk, OpPop]

interpretDeclaration :: Declaration -> ByteCodeGenT [OpCode]
interpretDeclaration (DeclVar (Decl var (Just expr))) = do
  result <- interpret expr
  env <- get
  if scope_depth env == 0 then return $ result ++ [OpDefineGlobal var]
    else do
    let scope = scope_depth env
    put $ env {locals = Local var scope:locals env}
    env' <- get
    liftIO $ print "in declaration\n"
    liftIO $ print $ show env'
    return result
interpretDeclaration (DeclVar (Decl var Nothing)) = do
  let result = [OpConstant NullValue]
  env <- get
  if scope_depth env == 0 then return $ result ++ [OpDefineGlobal var]
    else do
    let scope = scope_depth env
    put $ env {locals = Local var scope:locals env}
    return result

interpretDeclaration (DeclStatement stmt) = interpretStmt stmt

interpretDeclaration (DeclFun (Func !func_name !params !block)) = do
  -- void $ declareFunctionName func_name
  vm <- get
  let curr_scope = scope_depth vm
  x <- liftIO $ evalStateT (runExceptT (interpretAsBlock block)) (initEnvFunc func_name params curr_scope)
  liftIO $ print $ "funcbloc" ++ show x
  case x of
    Right block_codes -> buildFunction block_codes func_name
    Left x -> error $ show x
  where
    buildFunction :: [OpCode] -> T.Text -> ByteCodeGenT [OpCode]
    buildFunction opcodes func_name' = do
      let func_obj = FuncObj (L.length params) (Chunk $ Seq.fromList opcodes) func_name
      env <- get
      let func_opcode = OpConstant $ Function func_obj
      if scope_depth env == 0
        then return $ func_opcode : [OpDefineGlobal func_name']
        else do
        let scope = scope_depth env
        let all_locals = Local func_name' scope:locals env
        put $ env {locals = all_locals}
        return [func_opcode]

    declareFunctionName :: T.Text -> ByteCodeGenT [OpCode]
    declareFunctionName func_name = do
      env <- get
      if scope_depth env == 0
        then return [OpDefineGlobal func_name]
        else do
          let scope = scope_depth env
          put $ env {locals = Local func_name scope : locals env}
          return []


--   result <- interpretProgram block
--   let func_object = FuncObj {
--         funcboj_arity=L.length params
--         funcobj_chunk=result,
--         funcobj_name=func_name
--         }
--   env <- get
--   if scope_depth env == 0 then return $ [OpDefineGlobal var]
--     else do
--     let scope = scope_depth env
--     put $ env {locals = Local var scope:locals env}
--     return result

  --let !func = LoxValueFunction func_name params block closure
  --let !closure' = insertEnv func_name func closure -- capture the closure, add back func later on
  --put closure'
  --return LoxValueSentinel

-- interpretProgram :: Program -> ByteCodeGenT [OpCode]

interpretAsBlock program = do
  incrScopeDepth
  result <- interpretProgram program
  decrScopeDepth
  env <- get
  let l = locals env
  let l' = L.length $ L.filter (go env) l
  let locals' = L.filter (not . go env) l
  put $ env {locals = locals'}
  return $ result ++ L.replicate l' OpPop
  where
    go env (Local t offset) = offset > scope_depth env

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
