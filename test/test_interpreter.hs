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
import Data.Map as M


test_interpreter input expected = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = P.parse equality "" $ fromRight [] (scanner input)
  -- let result = interpret $ fromRight LoxNil x
  let (result, _) = runState (runExceptT (interpret $ fromRight LoxNil x)) M.empty
  expected @=? result

-- with this function we add back the final result of the script into the env and test the value of tat variable
test_program input lookup_key expected = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = fromRight [] $ P.parse loxProgram  "" $ fromRight [] (scanner input)
  env <- interpretProgram  x M.empty
  print $ show env
  let result = M.lookup lookup_key env
  case result of
    Just x' -> expected @=? x'
    Nothing -> assertFailure $ show env


test_errors input = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = P.parse equality "" $ fromRight [] (scanner input)
  let (result, _) = runState (runExceptT (interpret $ fromRight LoxNil x)) M.empty
  assertBool input (isLeft result)

test_expr = [
  test_interpreter "1>5;" $ Right (LoxValueBool False),
  test_interpreter "1+1;" $ Right (LoxValueDouble 2),
  test_interpreter "\"test\";" $ Right (LoxValueString "test"),
  test_interpreter "true" $ Right (LoxValueBool True),
  test_interpreter "nil" $ Right LoxValueNil,
  test_interpreter "(1+2)/2;" $ Right (LoxValueDouble 1.5),
  test_interpreter "(1>5)==(6<9);" $ Right (LoxValueBool False),
  test_interpreter "a;" $ Left "Unknown var: a",
  test_interpreter "var a;" $ Right LoxValueNil,
  test_errors "1>\"test\"",
  test_errors "1>5>6;",
  test_program "var a=10;var b=100;var c=a+b;print c;var result=c;" "result" (LoxValueDouble 110.0),
  test_program "var a=-1;var b=-a;var c=a+b;print c;var result=c;" "result" (LoxValueDouble 0.0),
  test_program "var a=-1;var b=-a;var c=a+b;print c;var result=a>b;" "result" (LoxValueBool False),
  test_program "var a=-1;var b=-a;var c=a+b;print c;var result=!(a>b);" "result" (LoxValueBool True)
  ]




main = do
  defaultMain $ testGroup "test_interpreter" test_expr
  --defaultMain tests
