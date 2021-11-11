{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text as T
import LispParser
import Data.String.QQ

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
  test_parser "(+ a b)" $ ExprPrim PrimAdd [ExprVar "a", ExprVar "b"],

  test_parser "(> a b)" $ ExprPrimPred PrimGt (ExprVar "a") (ExprVar "b"),
  test_parser "(< a b)" $ ExprPrimPred PrimLt (ExprVar "a") (ExprVar "b"),
  test_parser "(<= a b)" $ ExprPrimPred PrimLte (ExprVar "a") (ExprVar "b"),
  test_parser "(>= a b)" $ ExprPrimPred PrimGte (ExprVar "a") (ExprVar "b"),
  test_parser "(== a b)" $ ExprPrimPred PrimEq (ExprVar "a") (ExprVar "b"),
  test_parser "(== a b)" $ ExprPrimPred PrimEq  (ExprVar "a") (ExprVar "b"),

  test_parser "(&& a b)" $ ExprPrimPred PrimAnd (ExprVar "a") (ExprVar "b"),

  test_parser  "let a = 10 in let z = 10 in z * a" $ ExprLet (Identifier {unIdent = "a"},ExprLitNum 10) (ExprLet (Identifier {unIdent = "z"},ExprLitNum 10) (ExprVar "z")),

  test_parser "letrec f = (lambda (x) x) and g = (lambda (y) y) in (+ 2 3)" $ ExprLetRec [(Identifier {unIdent = "f"}, ExprLambda [Identifier {unIdent = "x"}] (ExprVar "x")), (Identifier {unIdent = "g"}, ExprLambda [Identifier {unIdent = "y"}] (ExprVar "y"))] (ExprPrim PrimAdd [ExprLitNum 2, ExprLitNum 3])

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
  lispInterpret "(if (- 5 1) (* 5 10) (+ 3 4))" (Right $ LispInt 50),

  lispInterpret "let a = 10 in let b=100 in (* a b)" $ Right (LispInt 1000),
  lispInterpret "let a = 10 in let b=(* a 2) in (* a b)" $ Right (LispInt 200),

  -- test shadowing
  lispInterpret "let a = 10 in let a=(* a 2) in a" $ Right (LispInt 20),

  lispInterpret "let c = 10 in let a = 20 in let z = (lambda (x y) (* x y)) in (z c a)" $ Right (LispInt 200),
  lispInterpret "let c = 10 in let a = 20 in let z = (lambda (x y) (* x y)) in (z (z c a) (z c a))" $ Right (LispInt 40000)
  ]

test_prog1 =
  let source =
        [s|let x = 5 in
        let x = 38 in
            let f = (lambda (y z) (* y (+ x z))) in
              let g = (lambda (u) (* u x)) in
                (f (g 3) 17)
      |]
   in lispInterpret source (Right (LispInt 6270))

test_prog2 =
  let source =
        [s|let makemult = (lambda (maker x)
                             (if x (+ 4 (maker maker (- x 1))) 0)) in
            let times4 = (lambda (x) (makemult makemult x)) in
                (times4 5)
          |]
   in lispInterpret source (Right (LispInt 20))

test_prog3 =
  let source =
        [s|let makemult = (lambda (maker x)
                             (if (== x 1) 1 (* x (maker maker (- x 1))))) in
            let times4 = (lambda (x) (makemult makemult x)) in
                (times4 5)
          |]
   in lispInterpret source (Right (LispInt 120))

test_prints = [
  test_print "a",
  test_print "(a y)",
  test_print "(lambda (x)\n  (a y))",
  test_print "(if (a y)\n    ((lambda (a)\n       (x a))\n      z)\n    ((lambda (x)\n       (c d))\n      z))",
  test_print "((lambda (x y z)\n  (a x))\n 1)",
  test_print "((lambda (x)\n  (+   a x f))\n 10)",
  test_print "(<= a b)",
  test_print "(&& a b)",
  test_print "(|| (lambda (x)\n  (if y\n      z\n      z)) (lambda (x y)\n  (+   1 2)))"
  ]

test_lisp_parsers = testGroup "test_lisp_parsers" $ test_exprs ++ test_prints ++ [test_prog1, test_prog2, test_prog3]

-- test_interpret = testGroup "test_lisp_interpret" $ test_lisp_interpreter

main = do
  defaultMain $ testGroup "test_lisp" [test_lisp_parsers, test_lisp_interpret]
  --defaultMain tests
