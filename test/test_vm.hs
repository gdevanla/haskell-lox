{-# LANGUAGE QuasiQuotes#-}
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
import Data.String.QQ

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

test_locals_simple = testCase "test_locals_simple" $ do
  let code = "var result1; var x1=200; {var x=10; {result1=x;}}"
  --let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result1", DValue 10.0),
            ("x1", DValue 200.0)
          ]
  assertEqual "" expected (globals vm)


test_set_locals_simple = testCase "test_set_locals_simple" $ do
  let code = "var result1; var x1=200; {var x=10; {x=15; result1=x;}}"
  --let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  --print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  let expected =
        M.fromList
          [ ("result1", DValue 15.0),
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
          [ ("result", DValue 10.0),
            ("result1", DValue 10.0)
          ]
  assertEqual "" expected (globals vm)

test_func_declaration = testCase "test_func_declaration" $ do
  let code = "var g=0; fun test_func(a) {var x = 20; g=x+a+x;} test_func(10);"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  -- print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  --print (vm_cf vm)
  let expected = M.fromList [("g", DValue 50.0)]
  assertEqual "" (M.delete "test_func" (globals vm)) expected


test_func_declaration_1 = testCase "test_func_declaration_1" $ do
  let code = "var g=0; fun test_func(a) {var x=20; g=x+a;} test_func(15);"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  -- print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  --print (vm_cf vm)
  let expected = M.fromList [("g", DValue 35.0)]
  assertEqual "" expected (M.delete "test_func" (globals vm))


test_func_declaration_2 = testCase "test_func_declaration_2" $ do
  let code = "var g=0; fun test_func(a) {var x=20; x = -5; g=x+a;} test_func(15);"
  -- let code = "var result1; result1=100; print result1;"
  opcodes' <- compileToByteCode . T.pack $ code
  -- print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  --print (vm_cf vm)
  let expected = M.fromList [("g", DValue 10.0)]
  assertEqual "" expected (M.delete "test_func" (globals vm))

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
          -- "var a; {fun func(z){var x=1; var y=20; print \"y=\"; print y; print \"x=\"; print x; a=x+y+z;}\nfunc(10)};",
          -- "var a=10; fun func(x, y){var z=x+y; print z; a=z;}\n func(10, 20);"
        ]
   in L.zip expressions expected

test_expressions =
  testGroup "test_expressions" $
    L.map (uncurry test_compiler) testData


test_while_loop_complex = testCase "test_while_loop_complex" $ do
  let source =
        [s|var x = 0;

        var result = 0;

        fun func(x1)
            {
              result = result + x1;
            }

        while (x < 5) {
              x = x + 1;
              if (x < 4) {
                 func(x);
               }
        }

        print result;

        |]

  opcodes' <- compileToByteCode . T.pack $ source
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  --print (vm_cf vm)
  liftIO $ print vm
  assertEqual "" (M.lookup "result" (globals vm)) (Just (DValue 6.0))

test_nested_function_call = testCase "test_nested_function_call" $ do
  let source =
        [s|
        var result = 0;

        fun func2(a)
                {
                  result = result + a
                }

        fun func(a)
            {
                func2(-2);
                {
                        var x = 2000;
                        func2(x);
                }
            }

        func2(10);
        func(10)

          |]
  opcodes' <- compileToByteCode . T.pack $ source
  print opcodes'
  let opcodes = fromRight [] opcodes'
  vm <- runInterpreter [Chunk (Seq.fromList opcodes)]
  --print (vm_cf vm)
  liftIO $ print vm
  assertEqual "" (M.lookup "result" (globals vm)) (Just (DValue 2008.0))

main = do
  defaultMain $
    testGroup "test_vm" $
      test_expressions :
      [ test_locals,
        test_locals_simple,
        test_set_locals_simple,
        test_conditional_if,
        test_conditional_just_if,
        test_conditional_else,
        test_conditional_and,
        test_conditional_and_false,
        test_conditional_or,
        test_while,
        test_while_false,
        test_func_declaration,
        test_func_declaration_1,
        test_func_declaration_2,
        test_while_loop_complex,
        test_nested_function_call

      ]

--defaultMain tests
