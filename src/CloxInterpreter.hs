{-# LANGUAGE RecordWildCards #-}
module CloxInterpreter where

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

import CloxByteCode

data VM = VM {
             chunk :: !Chunk,
             index:: !Int,  --probably not needed but closer to the book
             stack:: [Value]
             }
          deriving (Show, Eq)

push :: Value -> CloxIO ()
push value = do
  vm <- get
  put $ vm {stack=value:stack vm}

type CloxIO a = ExceptT T.Text (StateT VM IO) a

pop :: CloxIO Value
pop  = do
  vm <- get
  let x:xs = stack vm  -- handle exhaustive patterns while pop too many values
  put $ vm {stack=xs}
  return x

data InterpretResult =
  InterpretOK
  | InterpretCompileError
  | InterpretRuntimeError
  | InterpretNoResult


initVM :: Chunk -> VM
initVM chunk = VM {stack=[], chunk=chunk, index=0}

freeVM :: VM
freeVM = undefined

interpret :: CloxIO ()
interpret = do
  s <- get
  mapM_ interpretByteCode (unChunk . chunk $ s)

interpretByteCode :: OpCode -> CloxIO InterpretResult
interpretByteCode (OpConstant (DValue v)) = do
  liftIO $ print $ show v
  push (DValue v)
  -- liftIO $ putStrLn ("\n"::[Char])
  return InterpretNoResult
interpretByteCode OpReturn = return InterpretOK
interpretByteCode OpNegate = do
  (DValue v) <- pop
  liftIO $ print $ show (-v)
  return InterpretNoResult
interpretByteCode OpAdd = interpretBinOp (+)
interpretByteCode OpMinus = interpretBinOp (-)
interpretByteCode OpStar = interpretBinOp (*)
interpretByteCode OpSlash = interpretBinOp (/)
interpretByteCode OpExp = interpretBinOp (**)

interpretBinOp :: (Double -> Double -> Double) -> CloxIO InterpretResult
interpretBinOp func = do
  (DValue v1) <- pop
  (DValue v2) <- pop
  let result = func v1 v2
  push (DValue result)
  liftIO $ print $ show result
  return InterpretNoResult


runInterpreter :: IO ()
runInterpreter = do
  let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0))
  void $ (runStateT . runExceptT $ interpret) (initVM chunk)

  let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0) |> OpNegate)
  void $ (runStateT . runExceptT $ interpret) (initVM chunk)

  let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0) |> OpConstant (DValue 100.0) |> OpAdd)
  void $ (runStateT . runExceptT $ interpret) (initVM chunk)

  let chunk = Chunk (Seq.Empty |> OpConstant (DValue 2) |> OpConstant (DValue 2) |> OpAdd |> OpConstant (DValue 10) |> OpExp)
  void $ (runStateT . runExceptT $ interpret) (initVM chunk)
