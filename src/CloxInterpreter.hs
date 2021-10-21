{-# LANGUAGE RecordWildCards #-}
module CloxInterpreter where

import Control.Monad
import Control.Monad.State.Strict
import Text.Parsec.String
import Text.Parsec.Char
import qualified Text.Parsec as PS
import Text.Parsec.Combinator
import Control.Applicative
import Data.Either
import Data.Sequence as Seq
import System.IO (putStr)
import Data.Text as T
import Data.List as L
import Control.Monad.Except
import Data.Map.Strict as M

import CloxByteCode
import CloxCompiler

data CallFrame = CallFrame {
  cf_ip :: !Int,
  cf_funcobj:: FuncObj,
  cf_stack_offset:: !Int
  }
  deriving (Show, Eq)

newtype CallFrames = CallFrames {un_cf:: Seq CallFrame}
  deriving (Show, Eq)

data VM = VM {
             -- chunk :: !Chunk,
             -- ip:: !Int,
             vm_cf:: CallFrames,

             -- stack_index:: !Int,  -- we are going to use this to decide whether to skip instructions
             stack :: [Value],
             debugMode :: Bool,
             globals:: !(M.Map T.Text Value)

             }
          deriving (Show, Eq)

push :: Value -> CloxIO ()
push value = do
  vm <- get
  put $ vm {stack=value:stack vm}

type CloxIO a = ExceptT T.Text (StateT VM IO) a

pop :: CloxIO Value
pop = do
  vm <- get
  -- liftIO $ print vm
  -- liftIO $ print vm
  let x : xs = stack vm -- handle exhaustive patterns while pop too many values
  put $ vm {stack = xs}
  return x

peek :: CloxIO Value
peek = do
  vm <- get
  let x:_ = stack vm -- handle exhaustive patterns while pop too many values
  return x

peekN :: Int -> CloxIO Value
peekN offset = do
  vm <- get
  return $ (L.!!) (L.reverse $ stack vm) offset -- fix this, this is O(n)

peekBack :: Int -> CloxIO Value
peekBack offset = do
  vm <- get
  let offset' = L.length (stack vm) - offset - 1
  peekN offset'


setLocal :: Int -> Value -> CloxIO ()
setLocal offset value = do
  vm <- get
  let (xs, _:xs') = L.splitAt (offset-1) (L.reverse $ stack vm)
  let !s'= xs ++ [value] ++ xs'
  put $ vm {stack=L.reverse s'}

updateGlobals :: T.Text -> Value -> CloxIO ()
updateGlobals k v = do
  vm <- get
  let gm = globals vm
  let gm' = M.insert k v gm
  put $ vm {globals=gm'}

getGlobal :: T.Text -> CloxIO (Maybe Value)
getGlobal var = M.lookup var . globals <$> get

globalExists :: T.Text -> CloxIO Bool
globalExists var = M.member var . globals <$> get

data InterpretResult =
  InterpretOK
  | InterpretCompileError
  | InterpretRuntimeError !T.Text
  | InterpretNoResult


initVM :: Chunk -> VM
initVM chunk =
  let funcobj = FuncObj 0 chunk ""
      cf = CallFrame {cf_ip = 0, cf_funcobj = funcobj, cf_stack_offset = 0}
      cfs = CallFrames (cf <| Seq.empty)
   in VM
        { stack = [],
          debugMode = True,
          globals = M.empty,
          vm_cf = cfs
        }

addCFToVM :: FuncObj -> CloxIO ()
addCFToVM funcobj = do
  vm <- get
  let stacktop = L.length $ stack vm
  let argCount = funcobj_arity funcobj
  let offset = stacktop - argCount - 1
  let cf = CallFrame {cf_ip = 0, cf_funcobj = funcobj, cf_stack_offset = offset}
  let curr_cf = un_cf $ vm_cf vm
  let cfs = cf <| curr_cf
  put $ vm { vm_cf = CallFrames cfs }


moveIP :: Int -> CloxIO ()
moveIP offset = do
  vm <- get
  let cf:<|xs = un_cf $ vm_cf vm
  let cf' = cf {cf_ip=cf_ip cf + offset}
  put $ vm {vm_cf=CallFrames $ cf'<|xs}

incrIP = moveIP 1

getNextOpCode = do
  vm <- get
  let cf:<|_ = un_cf $ vm_cf vm
  let !opcode = Seq.lookup (cf_ip cf) (unChunk (funcobj_chunk (cf_funcobj cf)))
  return opcode

freeVM :: VM
freeVM = undefined

-- interpretByteCode' :: OpCode -> CloxIO InterpretResult
-- interpretByteCode' opcode = do
--   vm <- get
--   let current = stack_index vm
--   liftIO $ print vm
--   liftIO $ print opcode
--   if current == 0 then interpretByteCode opcode
--     else do
--     put $ vm {stack_index=current-1}
--     --vm <- get
--     --liftIO $ print (stack_index vm)
--     --liftIO $ print opcode
--     return InterpretNoResult

interpret :: CloxIO ()
interpret = go
  where
    go = do
      opcode <- getNextOpCode
      case opcode of
        Just oc -> do
          incrIP
          liftIO $ print $ "interpreting" ++ show opcode
          liftIO $ print oc
          void $ interpretByteCode oc
          go
        Nothing -> return ()


interpretByteCode :: OpCode -> CloxIO InterpretResult
interpretByteCode (OpConstant (DValue v)) = do
  --liftIO $ print $ show v
  --debugPrint v
  push (DValue v)
  -- vm <- get
  -- liftIO $ print $ "in const" ++ show v
  -- liftIO $ print vm
  -- liftIO $ putStrLn ("\n"::[Char])
  return InterpretNoResult
interpretByteCode (OpConstant (SValue v)) = do
  --liftIO $ print $ show v
  --debugPrint v
  push (SValue v)
  -- liftIO $ putStrLn ("\n"::[Char])
  return InterpretNoResult
interpretByteCode (OpConstant NullValue) = do
  --liftIO $ print $ show v
  --debugPrint v
  push NullValue
  return InterpretNoResult
interpretByteCode (OpConstant func) = do
    --liftIO $ print $ show v
    --debugPrint v
    push func
    -- liftIO $ putStrLn ("\n"::[Char])
    return InterpretNoResult
interpretByteCode OpReturn = return InterpretOK
interpretByteCode OpNegate = do
  (DValue v) <- pop
  let result = DValue (-v)
  --liftIO $ print $ show result
  push result
  return InterpretNoResult
interpretByteCode OpAdd = interpretBinOp (+)
interpretByteCode OpMinus = interpretBinOp (flip (-))
interpretByteCode OpStar = interpretBinOp (*)
interpretByteCode OpSlash = interpretBinOp (flip (/))
interpretByteCode OpExp = interpretBinOp (**)
interpretByteCode OpTrue = do
  push $ BValue True
  return InterpretNoResult
interpretByteCode OpFalse = do
  push $ BValue False
  return InterpretNoResult
interpretByteCode OpNull = do
  push NullValue
  return InterpretNoResult
interpretByteCode OpNot = do
  x <- pop
  case x of
    (BValue x) -> push $ BValue (not x)
    NullValue -> push $ BValue True
    _ -> push $ BValue False
  return InterpretNoResult
interpretByteCode OpEqual = do
  d1 <- pop
  d2 <- pop
  case (d1, d2) of
    (DValue x, DValue y) -> push $ BValue $ x == y
    (BValue x, BValue y) -> push $ BValue $ x == y
    _ -> push (BValue False)
  return InterpretNoResult
interpretByteCode OpGt = do
  d1 <- pop
  d2 <- pop
  case (d1, d2) of
    (DValue x, DValue y) -> push $ BValue $ y > x
    (BValue x, BValue y) -> push $ BValue $ y > x
    _ -> error $ "Cannot compare " ++ show d1 ++ "," ++ show d2 ++ " with >"
  return InterpretNoResult
interpretByteCode OpLt = do
  d1 <- pop
  d2 <- pop
  case (d1, d2) of
    (DValue x, DValue y) -> push $ BValue $ y < x
    (BValue x, BValue y) -> push $ BValue $ y < x
    _ -> error $ "Cannot compare " ++ show d1 ++ "," ++ show d2 ++ " with <"
  return InterpretNoResult
interpretByteCode OpPrint = do
  r <- pop
  liftIO $ print $ "printinggggggggggggggggggggggggggggg" ++ show r
  liftIO $ print r  -- need to have specialized print for Value type
  return InterpretNoResult
interpretByteCode (OpDefineGlobal var) = do
  vm <- get
  liftIO $ print $ "in define" ++ show var
  liftIO $ print vm
  val <- pop
  updateGlobals var val
  vm <- get
  liftIO $ print $ "in define" ++ show var
  liftIO $ print vm
  return InterpretNoResult
interpretByteCode (OpGetGlobal var) = do
  val <- getGlobal var
  case val of
    Just v -> do
      push v
      return InterpretNoResult
    Nothing -> return $ InterpretRuntimeError $ "Key Error" <> var
interpretByteCode (OpSetGlobal var) = do
  val <- pop
  exists <- globalExists var
  if exists then do
    updateGlobals var val
    return InterpretNoResult
    else return $ InterpretRuntimeError $ "Key assigned before declaration: " <> var
interpretByteCode (OpGetLocal i) = do
  vm <- get
  liftIO $ print "stack....\n"
  liftIO $ print $ stack vm
  liftIO $ print $ vm_cf vm
  val <- peekN i
  push val
  return InterpretNoResult
interpretByteCode (OpSetLocal i) = do
  val <- peek
  setLocal i val
  return InterpretNoResult
interpretByteCode OpPop = do
  void pop
  return InterpretNoResult
interpretByteCode (OpJumpIfFalse offset) = do
  val <- peekN 0
  vm <- get
  let is_truthy = isTruthy val
  unless is_truthy $ moveIP offset
  liftIO $ print $ "in false" ++ show val
  vm <- get
  liftIO $ print vm
  liftIO $ print offset
  return InterpretNoResult
interpretByteCode (OpJump offset) = do
  -- vm <- get
  -- liftIO $ print vm
  moveIP offset
  -- vm <- get
  --liftIO $ print vm
  return InterpretNoResult
interpretByteCode (OpLoopStart offset) = do
  --vm <- get
  --liftIO $ print vm
  moveIP (-offset)
  --vm <- get
  --liftIO $ print vm
  return InterpretNoResult
interpretByteCode (OpCall x) = do
  funcobj <- peekBack x
  vm <- get
  case funcobj of
    Function fo@FuncObj{..} -> do
      liftIO $ print "printing fo"
      liftIO $ print vm
      addCFToVM fo
      vm <- get
      liftIO $ print "printing fo after"
      liftIO $ print vm
      liftIO $ print fo
      interpret
      return InterpretNoResult
    _ -> error $ "got non-callable" ++ show funcobj


interpretByteCode x = do
  vm <- get
  error $ "not supported" ++ show x ++ show vm

isTruthy :: Value -> Bool
isTruthy (DValue _) = True
isTruthy (BValue True) = True
isTruthy (BValue False) = False
isTruthy (SValue _) = True
isTruthy NullValue = False


interpretBinOp :: (Double -> Double -> Double) -> CloxIO InterpretResult
interpretBinOp func = do
  (DValue v1) <- pop
  (DValue v2) <- pop
  let result = func v1 v2
  push (DValue result)
  -- liftIO $ print $ show result
  -- debugPrint result
  return InterpretNoResult

debugPrint :: (Show a)=> a -> CloxIO ()
debugPrint r = do
  s <- get
  when (debugMode s) $ liftIO $ print r

-- runInterpreter :: IO ()
-- runInterpreter = do
--   let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0))
--   void $ (runStateT . runExceptT $ interpret) (initVM chunk)

--   let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0) |> OpNegate)
--   void $ (runStateT . runExceptT $ interpret) (initVM chunk)

--   let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0) |> OpConstant (DValue 100.0) |> OpAdd)
--   void $ (runStateT . runExceptT $ interpret) (initVM chunk)

--   let chunk = Chunk (Seq.Empty |> OpConstant (DValue 2) |> OpConstant (DValue 2) |> OpAdd |> OpConstant (DValue 10) |> OpExp)
--   void $ (runStateT . runExceptT $ interpret) (initVM chunk)

runInterpreter :: [Chunk] -> IO VM
runInterpreter chunk = do
  ((a, s) : _) <- mapM ((runStateT . runExceptT $ interpret) . initVM) chunk
  -- print s
  return s
