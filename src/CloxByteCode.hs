{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module CloxByteCode where

import Data.Foldable
import Data.Sequence
import Data.Text as T

data Value = DValue {-# UNPACK #-} !Double
             | BValue {-# UNPACK #-} !Bool
             | SValue !T.Text
             | NullValue
           deriving (Show, Eq)

data OpCode
  = OpReturn
  | OpConstant !Value
  | OpNegate
  | OpAdd
  | OpMinus
  | OpStar
  | OpSlash
  | OpExp
  | OpTrue
  | OpFalse
  | OpNull
  | OpNot
  | OpEqual
  | OpGt
  | OpLt
  | OpPrint
  | OpPop
  | OpDefineGlobal !T.Text  -- Not storing this in constant unlike in the book
  | OpGetGlobal !T.Text -- Not storing this in constant unlike in the book
  | OpSetGlobal !T.Text
  | OpSetLocal !Int
  | OpGetLocal !Int
  | OpJumpIfFalse !Int
  | OpJump !Int
  deriving (Show, Eq)

newtype Chunk = Chunk {unChunk :: Seq OpCode}
  deriving (Show, Eq)

writeChunk :: Chunk -> OpCode -> Chunk
writeChunk (Chunk chunk) opcode = Chunk (chunk |> opcode)

disassembleChunk :: Chunk -> T.Text -> (T.Text, T.Text)
disassembleChunk (Chunk !chunk) !label =
  let instr = T.unlines . toList $ mapWithIndex (flip disInstr) chunk
   in ("====" <> label <> "====\n", instr)

disInstr :: OpCode -> Int -> T.Text
disInstr op_code =  render $ T.pack $ show op_code

render :: T.Text -> Int -> T.Text
render bytecode offset = T.justifyRight 4 '0' (T.pack (show offset)) <> " " <> bytecode
