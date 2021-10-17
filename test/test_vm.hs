{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import CloxByteCode
--import CloxCompiler
import CloxByteCodeGen
import CloxInterpreter
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Sequence
import Data.Sequence as Seq
import Data.Text as T
import ExprInterpreter
import ExprParser
import RIO
import Scanner
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec as P

test_compiler input expected = testCase input $ do
  opcodes' <- compileToByteCode . T.pack $ input
  let opcodes = fromRight [] $ opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  -- let actual_stack = stack vm
  assertEqual "" (M.fromList expected) (globals vm)

test_locals = testCase "test_locals" $ do
  let code = "var result1; var result2; var result3; var x1=200; {var x=10; {result1=x1+x; x=15; result2=x1+x;} result3=x;}"
  --let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result1", DValue 210.0),
            ("result2", DValue 215.0),
            ("result3", DValue 15.0),
            ("x1", DValue 200.0)
          ]
  assertEqual "" expected (globals vm)

test_conditional_if = testCase "test_conditional_if" $ do
  let code = "var result;var x = 10; if (x==10) {result=true;} else {result=false;}"
  --let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result", BValue True),
            ("x", DValue 10.0)
          ]
  assertEqual "" expected (globals vm)

test_conditional_else = testCase "test_conditional_else" $ do
  let code = "var result;var x = 10; if (x==11) {result=true;} else {result=false;}"
  --let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result", BValue False),
            ("x", DValue 10.0)
          ]
  assertEqual "" expected (globals vm)

test_conditional_just_if = testCase "test_conditional_just_if" $ do
  let code = "var result;var x = 10; if (x==11) {result=true;var y = 10;} var result1=100;"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result", NullValue),
            ("x", DValue 10.0),
            ("result1", DValue 100.0)
          ]
  assertEqual "" expected (globals vm)

test_conditional_and = testCase "test_conditional_and" $ do
  let code = "var result=10; var result1=(result==10 and result < 11);"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result", DValue 10.0),
            ("result1", BValue True)
          ]

  assertEqual "" expected (globals vm)

test_conditional_and_false = testCase "test_conditional_and" $ do
  let code = "var result=10; var result1=(result==10 and result < 5);"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result", DValue 10.0),
            ("result1", BValue False)
          ]

  assertEqual "" expected (globals vm)

test_conditional_or = testCase "test_conditional_or" $ do
  let code = "var result=10; var result1=(false or true);"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result", DValue 10.0),
            ("result1", BValue True)
          ]

  assertEqual "" expected (globals vm)

test_while = testCase "test_while" $ do
  let code = "var result=10; while (result<12) { result = result + 1;}"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [("result", DValue 12.0)]
  assertEqual "" expected (globals vm)

test_while_false = testCase "test_while_false" $ do
  let code = "var result=10; while (result<5) { result = result + 1;} var result1=result;"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [("result", DValue 10.0),
           ("result1", DValue 10.0)]
  assertEqual "" expected (globals vm)


testData =
  let expected =
        [ [("x", DValue 10.0)],
          [("x", DValue 9.0)],
          [("x", DValue 4.0)],
          [("x", DValue 8.0)],
          [("x", DValue 28.0)],
          [("x", DValue 22.0)],
          [("x", DValue 1.0)],
          [("x", BValue True)],
          [("x", BValue True)],
          [("x", BValue True)],
          [("x", BValue True)],
          [("x", BValue False)],
          [("x", BValue True)],
          [("x", BValue True)],
          [("x", SValue "test_var")],
          [],
          [("x", SValue "print this")], -- print statement, nothing on stack
          [("x", DValue (-20.0))], -- var and print
          [("x", DValue (-40.0))], -- var and print
          [("x", DValue (-10)), ("y", DValue (-20))]
        ]
      expressions =
        [ "var x=1+2+3+4;",
          "var x=10-2+1;",
          "var x=10-5-1;",
          "var x=10+2*3-8;",
          "var x=(10+2)*3-8;",
          "var x=10*2+4/2;",
          "var x=10/2-4;",
          "var x=true;",
          "var x=1<2;",
          "var x=2>1;",
          "var x=5==5;",
          "var x=5!=5;",
          "var x=5<=5;",
          "var x=5>=5;",
          "var x=\"test_var\";",
          "print 10000+20000;",
          "var x = \"print this\";print x;",
          "var x=-10;x=x+x;",
          "var x=-10;x=-20;x=x+x;",
          "var x=-10;var y=-20;"
        ]
   in L.zip expressions expected

test_expressions =
  testGroup "test_expressions" $
    L.map (uncurry test_compiler) testData

main = do
  defaultMain $ testGroup "test_vm" $ test_expressions : [test_locals, test_conditional_if, test_conditional_just_if, test_conditional_else, test_conditional_and, test_conditional_and_false, test_conditional_or, test_while, test_while_false]

--defaultMain tests
