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
  cf_ip :: {-#UNPACK#-} !Int,
  cf_funcobj:: !FuncObj,
  cf_stack_offset:: !Int
  }
  deriving (Show, Eq)

newtype CallFrames = CallFrames {un_cf:: Seq CallFrame}
  deriving (Show, Eq)

data VM = VM {
             -- chunk :: !Chunk,
             -- ip:: !Int,
             vm_cf:: !CallFrames,

             -- stack_index:: !Int,  -- we are going to use this to decide whether to skip instructions
             -- stack :: [Value],
             stack :: !(Seq Value),
             debugMode :: !Bool,
             globals:: !(M.Map T.Text Value)

             }
          deriving (Show, Eq)

push :: Value -> CloxIO ()
push value = do
  vm <- get
  put $ vm {stack=value:<|stack vm}

type CloxIO a = ExceptT T.Text (StateT VM IO) a

pop :: CloxIO Value
pop = do
  vm <- get
  -- liftIO $ print vm
  -- liftIO $ print vm
  let x :<| xs = stack vm -- handle exhaustive patterns while pop too many values
  put $ vm {stack = xs}
  return x

peek :: CloxIO Value
peek = do
  vm <- get
  let x:<|_ = stack vm -- handle exhaustive patterns while pop too many values
  return x

peekN :: Int -> CloxIO Value
peekN offset = do
  vm <- get
  top_cf <- peekCF
  let so = cf_stack_offset top_cf + offset
  --return $ (L.!!) (L.reverse $ stack vm) (so + offset) -- fix this, this is O(n)
  return $ Seq.index (stack vm) (Seq.length (stack vm) - so - 1)

peekCF :: CloxIO CallFrame
peekCF = do
  vm <- get
  let cf :<| _ = un_cf $ vm_cf vm
  return cf

popCF :: CloxIO Value
popCF = do
  vm <- get
  let cf :<| cfs = un_cf $ vm_cf vm
  result <- peek
  --let stack' = L.reverse $ L.take (cf_stack_offset cf) (L.reverse $ stack vm)
  let stack' = Seq.drop (Seq.length (stack vm) - cf_stack_offset cf) (stack vm)
  put $ vm {vm_cf = CallFrames cfs, stack=stack'}
  return result

peekBack :: Int -> CloxIO Value
peekBack offset = do
  vm <- get
  let so = Seq.length (stack vm) - offset - 1
  -- return $ (L.!!) (L.reverse $ stack vm) (offset') -- fix this, this is O(n)
  return $ Seq.index (stack vm) (Seq.length (stack vm) - so - 1)
  -- peekN offset'

setLocal :: Int -> Value -> CloxIO ()
setLocal offset value = do
  vm <- get
  --liftIO $ print $ "before set local = " ++ show offset
  --liftIO $ print (stack vm)
  cf <- peekCF
  let so = Seq.length (stack vm) - cf_stack_offset cf  - offset - 1
  let stack' = Seq.update so value (stack vm)
  --let (xs, _:xs') = L.splitAt so (L.reverse $ stack vm)
  --let !s'= xs ++ [value] ++ xs'
  -- liftIO $ print (s')
  put $ vm {stack=stack'}

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
        { stack = Function funcobj:<|Seq.empty,
          debugMode = True,
          globals = M.empty,
          vm_cf = cfs
        }

addCFToVM :: FuncObj -> CloxIO ()
addCFToVM funcobj = do
  vm <- get
  -- liftIO $ print "adding CF to VM"
  let stacktop = L.length $ stack vm
  -- liftIO $ print vm
  let argCount = funcobj_arity funcobj
  let offset = stacktop - argCount - 1
  -- liftIO $ print $ show stacktop ++ " " ++ show (funcobj_arity funcobj)
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
{-# INLINE moveIP #-}


incrIP = moveIP 1
{-#INLINE incrIP#-}


getNextOpCode = do
  vm <- get
  let cf:<|_ = un_cf $ vm_cf vm
  let !opcode = Seq.lookup (cf_ip cf) (unChunk (funcobj_chunk (cf_funcobj cf)))
  return opcode
{-# INLINE getNextOpCode #-}

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
          -- liftIO $ print $ "interpreting" ++ show opcode
          -- liftIO $ print oc
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
interpretByteCode OpReturn = do
  result <- popCF
  push result
  return InterpretOK
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
  -- liftIO $ print $ "printing == " ++ show r
  liftIO $ print r  -- need to have specialized print for Value type
  return InterpretNoResult
interpretByteCode (OpDefineGlobal var) = do
  vm <- get
  -- liftIO $ print $ "in define" ++ show var
  -- liftIO $ print vm
  val <- pop
  updateGlobals var val
  vm <- get
  -- liftIO $ print $ "in define" ++ show var
  -- liftIO $ print vm
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
  -- liftIO $ print "stack....\n"
  -- liftIO $ print $ stack vm
  -- liftIO $ print $ vm_cf vm
  -- liftIO $ print $ "in OpGetLocal = " ++ show i
  val <- peekN i
  -- liftIO $ print $ "in OpGetLocal, got = " ++ show val
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
  val <- peek
  vm <- get
  let is_truthy = isTruthy val
  unless is_truthy $ moveIP offset
  -- liftIO $ print $ "in false" ++ show val
  vm <- get
  -- liftIO $ print vm
  -- liftIO $ print offset
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
  -- liftIO $ print vm
  -- liftIO $ print funcobj
  -- liftIO $ print (stack vm)
  case funcobj of
    Function fo@FuncObj{..} -> do
      -- liftIO $ print "printing fo"
      -- liftIO $ print vm
      -- liftIO $ print funcobj
      addCFToVM fo
      --vm <- get
      -- liftIO $ print "printing fo after"
      -- liftIO $ print vm
      -- liftIO $ print fo
      -- liftIO $ print (stack vm)
      --interpret
      --popCF
      -- interpret
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
isTruthy x = error $ show x


interpretBinOp :: (Double -> Double -> Double) -> CloxIO InterpretResult
interpretBinOp func = do
  vm <- get
  ---- liftIO $ print (stack vm)
  (DValue v1) <- pop
  (DValue v2) <- pop
  let result = func v1 v2
  push (DValue result)
  -- -- liftIO $ print $ show result
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
