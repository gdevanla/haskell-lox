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


test_interpreter input expected = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = P.parse equality "" $ fromRight [] (scanner input)
  -- let result = interpret $ fromRight LoxNil x
  (result, _) <- liftIO $ runStateT (runExceptT (interpret $ fromRight LoxNil x)) (initEnv Nothing)
  expected @=? result

-- with this function we add back the final result of the script into the env and test the value of tat variable
test_program input lookup_key expected = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = fromRight [] $ P.parse loxProgram "" $ fromRight [] (scanner input)
  (lox_result, env) <- liftIO $ runStateT (runExceptT (interpretProgram x)) (initEnv Nothing)
  -- print $ show env
  -- print $ show msg
  let result = lookupEnv lookup_key env
  case result of
    Just x' -> expected @=? x'
    Nothing -> assertFailure $ show lox_result ++ show env


test_program_error input expected = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = fromRight [] $ P.parse loxProgram "" $ fromRight [] (scanner input)
  (result, _) <- liftIO $ runStateT (runExceptT (interpretProgram x)) (initEnv Nothing)
  -- case msg of
  --   Just msg' -> expected @=? msg'
  --   Nothing -> assertFailure $ show env
  assertBool input (isLeft result)

test_errors input = testCase input $ do
  --let result = either (const LoxValueNil) $
  let x = P.parse equality "" $ fromRight [] (scanner input)
  (result, _) <- liftIO $ runStateT (runExceptT (interpret $ fromRight LoxNil x)) (initEnv Nothing)
  assertBool input (isLeft result)


test_env = testCase "test_env"  $ do
  let root_env'' = insertEnv "a" (LoxValueDouble 1.0) (initEnv Nothing)
  let root_env' = insertEnv "z" (LoxValueDouble 100.0) root_env''
  let root_env = insertEnv "w" (LoxValueDouble 200.0) root_env'
  let child_env = insertEnv "a" (LoxValueDouble 2.0) (initEnv (Just root_env))
  let child_env' = insertEnv "b" (LoxValueDouble 3.0) child_env

  let grand_child = insertEnv "d" (LoxValueDouble 4.0) (initEnv (Just child_env'))
  let grand_child' = insertEnv "a" (LoxValueDouble 5.0) grand_child

  Just (LoxValueDouble 1.0) @=? lookupEnv "a" root_env
  Just (LoxValueDouble 2.0) @=? lookupEnv "a" child_env'
  Just (LoxValueDouble 3.0) @=? lookupEnv "b" child_env'
  Just (LoxValueDouble 4.0) @=? lookupEnv "d" grand_child'
  Just (LoxValueDouble 5.0) @=? lookupEnv "a" grand_child'
  Just (LoxValueDouble 3.0) @=? lookupEnv "b" grand_child'

  let grand_child'' = fromJust $ updateEnv "z" (LoxValueBool True) grand_child'

  Just (LoxValueDouble 5.0) @=? lookupEnv "a" grand_child''
  Just (LoxValueDouble 3.0) @=? lookupEnv "b" grand_child''
  Just (LoxValueDouble 4.0) @=? lookupEnv "d" grand_child''
  Just (LoxValueDouble 5.0) @=? lookupEnv "a" grand_child''
  Just (LoxValueBool True) @=? lookupEnv "z" grand_child''

  return ()





test_interpreters = [
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
  test_program "var a=-1;var b=-a;var c=a+b;print c;var result=!(a>b);" "result" (LoxValueBool True),
  test_program "var x;var a;var b;var c;a=b = c= 10;x=a+b+c;" "x" (LoxValueDouble 30.0),
  test_program_error "a=b = c= 10;x=a+b+c;" "Assignment to variable before declaration c",

  -- test env
  test_env,

  -- test block
  test_program "var a=10; var b=20; var result; {var b=100; result=b+a; print result;};" "result" (LoxValueDouble 110.0),

  test_program "var a=10; var b=20; var result; {var b=100; result=b+a;} result=a+b;print result;" "result" (LoxValueDouble 30.0),

  -- ifelse conditions
  test_program "var a=10;var result;if (a!=10) {result=10;} else {result=20;} print result;" "result" (LoxValueDouble 20.0),

  test_program "var a=10;var result;if (a==10) {result=100;} else {result=20;} print result;" "result" (LoxValueDouble 100.0),


    test_program "var result=true and true;" "result" (LoxValueBool True),
    test_program "var result=true and false;" "result" (LoxValueBool False),
    test_program "var result=false and false;" "result" (LoxValueBool False),
    test_program "var result=false and true;" "result" (LoxValueBool False),

    test_program "var result=true or true;" "result" (LoxValueBool True),
    test_program "var result=true or false;" "result" (LoxValueBool True),
    test_program "var result=false or false;" "result" (LoxValueBool False),
    test_program "var result=false or true;" "result" (LoxValueBool True),

    -- check precdence
    test_program "var result=false or true and true;" "result" (LoxValueBool True),
    test_program "var result=true or true and false;" "result" (LoxValueBool True),

    test_program "var result=0; while(result<10) {result=result+1;}" "result" (LoxValueDouble 10.0)

  ]




main = do
  defaultMain $ testGroup "test_interpreter" test_interpreters
  --defaultMain tests
