{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Scanner
import ExprParser
-- import ExprInterpreter

import Text.Parsec as P
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Maybe
import Data.List as L
import Data.Text as T

import CloxByteCode
import CloxByteCodeGen
import CloxCompiler
import Scanner
import ExprParser

import Data.Sequence

test_compiler input expected = testCase input $ do
  let x = fromRight LoxNil $ P.parse equality "" $ fromRight [] (scanner input)
  print x
  (opcodes, _) <- liftIO $ runStateT (runExceptT (interpret x)) (initEnv)
  -- putStrLn opcodes

  -- let opcodes = fromRight [] $ compileToByteCode . T.pack $ input
  -- putStrLn . show $ opcodes
  assertEqual "" (fromRight [] opcodes) expected

testData = let
  expected = [
    [OpConstant (DValue 1.0), OpConstant (DValue 2.0), OpAdd, OpConstant (DValue 3.0), OpAdd, OpConstant (DValue 4.0), OpAdd],
    [OpConstant (DValue 10.0), OpConstant (DValue 2.0), OpMinus, OpConstant (DValue 1.0), OpAdd],
    [OpConstant (DValue 10.0), OpConstant (DValue 5.0), OpMinus, OpConstant (DValue 1.0), OpMinus],
    [OpConstant (DValue 10.0), OpConstant (DValue 2.0), OpConstant (DValue 3.0), OpStar, OpAdd, OpConstant (DValue 8.0), OpMinus],
    [OpConstant (DValue 10.0), OpConstant (DValue 2.0), OpAdd, OpConstant (DValue 3.0), OpStar, OpConstant (DValue 8.0), OpMinus],
    []
    ]
  expressions = [
      "1+2+3+4;",
      "10-2+1;",
      "10-5-1;",
      "10+2*3-8;",
      "(10+2)*3-8;",
      ]
  in
  L.zip expressions expected


test_expressions = testGroup "test_expressions" $
  L.map (uncurry test_compiler) testData

main = do
  defaultMain $ testGroup "test_bytecode_gen"
    [test_expressions]
  --defaultMain tests
