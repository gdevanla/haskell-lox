{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import LispParser
import Text.Parsec as P

test_parser input expected = testCase input $ do
  let result = lexAndParse input
  case result of
    Right r -> expected @=? r
    Left e -> error $ show e

test_exprs = [
  test_parser "a" (ExprVar "a"),
  test_parser "(a y)" (ExprApp (ExprVar "a") [ExprVar "y"]),
  test_parser
    "((lambda (x) (a x)) 1)"
    (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "a") [ExprVar "x"])) [ExprLitNum 1]),
  test_parser "(if (a y) ((lambda (a) (x a)) z) ((lambda (x) (c d)) z))"
    (ExprIf (ExprApp (ExprVar "a") [ExprVar "y"])
            (ExprApp (ExprLambda [Identifier "a"] (ExprApp (ExprVar "x") [ExprVar "a"])) [ExprVar "z"])
            (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "c") [ExprVar "d"])) [ExprVar "z"]))
  ]


test_parsers = test_exprs

main = do
  defaultMain $ testGroup "test_lisp_parser" test_parsers
  --defaultMain tests
