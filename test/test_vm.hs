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
import Data.Map as M
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

test_locals = testCase "test_locals" $ do
  let code = "var result1; var result2; var result3; var x1=200; {var x=10; {result1=x1+x; x=15; result2=x1+x;} result3=x;}"
  --let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected = M.fromList [
        ("result1", DValue 210.0),
        ("result2", DValue 215.0),
        ("result3", DValue 15.0),
        ("x1", DValue 200.0)]
  assertEqual "" expected (globals vm)


testData = let
  expected = [
    [DValue 10.0],
    [DValue 9.0],
    [DValue 4.0],
    [DValue 8.0],
    [DValue 28.0],
    [DValue 22.0],
    [DValue 1.0],
    [BValue True],
    [BValue True],
    [BValue True],
    [BValue True],
    [BValue False],
    [BValue True],
    [BValue True],
    [SValue "test_var"],
    [], -- print statement, nothing on stack
    [], -- global definition
    [], -- var and print
    [DValue (-20.0)], -- var and print
    [DValue (-40.0)], -- var and print
    []
    ]
  expressions = [
      "1+2+3+4;",
      "10-2+1;",
      "10-5-1;",
      "10+2*3-8;",
      "(10+2)*3-8;",
      "10*2+4/2;",
      "10/2-4;",
      "true;",
      "1<2;",
      "2>1;",
      "5==5;",
      "5!=5;",
      "5<=5;",
      "5>=5;",
      "\"test_var\";",
      "print 10000+20000;",
      "var x = 100;", -- TODO: Add test to check the global hash table
      "var x = \"print this\";print x;", -- TODO: Add test to check the global hash table
      "var x=-10;x+x;", -- TODO: Add test to check the global hash table
      "var x=-10;x=-20;x+x;", -- TODO: Add test to check the global hash table
      "var x=-10;y=-20;" -- TODO: Check Interpreter return value for RunTimeError
    ]
  in
  L.zip expressions expected


test_expressions = testGroup "test_expressions" $
  L.map (uncurry test_compiler) testData

main = do
  defaultMain $ testGroup "test_vm" $ test_expressions:[test_locals]
  --defaultMain tests
