{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Scanner
import ExprParser
import ExprInterpreter

import Text.Parsec as P
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Maybe
import Data.List as L
import Data.Text as T
import Data.Sequence as Seq

import CloxByteCode
--import CloxCompiler
import CloxByteCodeGen
import CloxInterpreter


import Data.Sequence

test_compiler input expected = testCase input $ do
  opcodes' <- compileToByteCode . T.pack $ input
  let opcodes = fromRight [] $ opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let actual_stack = stack vm
  assertEqual "" expected actual_stack

testData = let
  expected = [
    [DValue 10.0],
    [DValue 9.0],
    [DValue 4.0],
    [DValue 8.0],
    [DValue 28.0],
    [DValue 22.0],
    [DValue 1.0]
    ]
  expressions = [
      "1+2+3+4;",
      "10-2+1;",
      "10-5-1;",
      "10+2*3-8;",
      "(10+2)*3-8;",
      "10*2+4/2;",
      "10/2-4;"
    ]
  in
  L.zip expressions expected


test_expressions = testGroup "test_expressions" $
  L.map (uncurry test_compiler) testData

main = do
  defaultMain $ testGroup "test_vm"
    [test_expressions]
  --defaultMain tests
