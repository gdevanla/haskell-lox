{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import LispParser
import Text.Parsec as P
import Data.Text as T

test_parser input expected = testCase input $ do
  let result = lexAndParse input
  case result of
    Right r -> expected @=? r
    Left e -> error $ show e


test_print input = testCase input $ do
  let result = lexAndParse input
  case result of
    Right r -> input @=? T.unpack (printExpr r 0)
    Left e -> error $ show e

test_exprs = [
  test_parser "a" (ExprVar "a"),
  test_parser "(a y)" (ExprApp (ExprVar "a") [ExprVar "y"]),
  test_parser
    "((lambda (x) (a x)) 1)"
    (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "a") [ExprVar "x"])) [ExprLitNum 1]),

  test_parser
    "((lambda (x y z) (a x)) 1)"
    (ExprApp (ExprLambda [Identifier "x", Identifier "y", Identifier "z"] (ExprApp (ExprVar "a") [ExprVar "x"])) [ExprLitNum 1]),

  test_parser "(if (a y) ((lambda (a) (x a)) z) ((lambda (x) (c d)) z))"
    (ExprIf (ExprApp (ExprVar "a") [ExprVar "y"])
            (ExprApp (ExprLambda [Identifier "a"] (ExprApp (ExprVar "x") [ExprVar "a"])) [ExprVar "z"])
            (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "c") [ExprVar "d"])) [ExprVar "z"]))
  ]

test_prints = [
  test_print "a",
  test_print "(a y)",
  test_print "(lambda (x)\n  (a y))",
  test_print "(if (a y)\n    ((lambda (a)\n       (x a))\n      z)\n    ((lambda (x)\n       (c d))\n      z))",
  test_print "((lambda (x y z)\n  (a x))\n 1)"
  ]

test_parsers = test_exprs ++ test_prints

main = do
  defaultMain $ testGroup "test_lisp_parser" test_parsers
  --defaultMain tests
