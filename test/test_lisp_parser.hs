{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text as T
import LispParser

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
            (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "c") [ExprVar "d"])) [ExprVar "z"])),
  test_parser "(+ a b)" $ ExprPrim PrimAdd [ExprVar "a", ExprVar "b"]
  ]


lispInterpret input expected = testCase input $ do
  result <- runInterpreter input
  putStrLn $ show result
  expected @=? result

test_lisp_interpret = testGroup "test_list_interpret" [
  lispInterpret "(+ 1 2)" (Right $ LispInt 3),
  lispInterpret "(+ 5 (+ 1 2))" (Right $ LispInt 8),
  lispInterpret "(+ 5  3)" (Right $ LispInt 8),
  lispInterpret "(+ (+ 5 1)  (+ 6 1) 6)" (Right $ LispInt 19),
  lispInterpret "(+ (+ 5 10) (- 6 1) 6)" (Right $ LispInt 26),
  lispInterpret "(if 0 (* 5 10) (+ 3 4))" (Right $ LispInt 7),
  lispInterpret "(if 1 (* 5 10) (+ 3 4))" (Right $ LispInt 50)
  ]


test_prints = [
  test_print "a",
  test_print "(a y)",
  test_print "(lambda (x)\n  (a y))",
  test_print "(if (a y)\n    ((lambda (a)\n       (x a))\n      z)\n    ((lambda (x)\n       (c d))\n      z))",
  test_print "((lambda (x y z)\n  (a x))\n 1)",
  test_print "((lambda (x)\n  (+   a x f))\n 10)"
  ]

test_lisp_parsers = testGroup "test_lisp_parsers" $ test_exprs ++ test_prints

-- test_interpret = testGroup "test_lisp_interpret" $ test_lisp_interpreter

main = do
  defaultMain $ testGroup "test_lisp" [test_lisp_parsers, test_lisp_interpret]
  --defaultMain tests
