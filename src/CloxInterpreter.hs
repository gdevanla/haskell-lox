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

push vm@VM{..} value = vm {stack=value:stack}

type CloxIO a = ExceptT T.Text (StateT VM IO) a

pop vm@VM{..} = let
  x:xs = stack
  in
  (vm {stack=stack}, x)

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
  -- liftIO $ putStrLn ("\n"::[Char])
  return InterpretNoResult
interpretByteCode OpReturn = return InterpretOK

runInterpreter :: IO ()
runInterpreter = do
  let chunk = Chunk (Seq.Empty |> OpConstant (DValue 100.0))
  void $ (runStateT . runExceptT $ interpret) (initVM chunk)
