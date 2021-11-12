{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.String.QQ
import Data.Text as T
import LispParser
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.List as L

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

test_exprs =
  [ test_parser "a" (ExprVar "a"),
    test_parser "(a y)" (ExprApp (ExprVar "a") [ExprVar "y"]),
    test_parser
      "((lambda (x) (a x)) 1)"
      (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "a") [ExprVar "x"])) [ExprLitNum 1]),
    test_parser
      "((lambda (x y z) (a x)) 1)"
      (ExprApp (ExprLambda [Identifier "x", Identifier "y", Identifier "z"] (ExprApp (ExprVar "a") [ExprVar "x"])) [ExprLitNum 1]),
    test_parser
      "(if (a y) ((lambda (a) (x a)) z) ((lambda (x) (c d)) z))"
      ( ExprIf
          (ExprApp (ExprVar "a") [ExprVar "y"])
          (ExprApp (ExprLambda [Identifier "a"] (ExprApp (ExprVar "x") [ExprVar "a"])) [ExprVar "z"])
          (ExprApp (ExprLambda [Identifier "x"] (ExprApp (ExprVar "c") [ExprVar "d"])) [ExprVar "z"])
      ),
    test_parser "(+ a b)" $ ExprPrim PrimAdd [ExprVar "a", ExprVar "b"],
    test_parser "(> a b)" $ ExprPrimPred PrimGt (ExprVar "a") (ExprVar "b"),
    test_parser "(< a b)" $ ExprPrimPred PrimLt (ExprVar "a") (ExprVar "b"),
    test_parser "(<= a b)" $ ExprPrimPred PrimLte (ExprVar "a") (ExprVar "b"),
    test_parser "(>= a b)" $ ExprPrimPred PrimGte (ExprVar "a") (ExprVar "b"),
    test_parser "(== a b)" $ ExprPrimPred PrimEq (ExprVar "a") (ExprVar "b"),
    test_parser "(== a b)" $ ExprPrimPred PrimEq (ExprVar "a") (ExprVar "b"),
    test_parser "(&& a b)" $ ExprPrimPred PrimAnd (ExprVar "a") (ExprVar "b"),
    test_parser "let a = 10 in let z = 10 in z * a" $ ExprLet (Identifier {unIdent = "a"}, ExprLitNum 10) (ExprLet (Identifier {unIdent = "z"}, ExprLitNum 10) (ExprVar "z")),
    test_parser "letrec f = (lambda (x) x) and g = (lambda (y) y) in (+ 2 3)" $ ExprLetRec [(Identifier {unIdent = "f"}, ExprLambda [Identifier {unIdent = "x"}] (ExprVar "x")), (Identifier {unIdent = "g"}, ExprLambda [Identifier {unIdent = "y"}] (ExprVar "y"))] (ExprPrim PrimAdd [ExprLitNum 2, ExprLitNum 3]),
    test_parser "(try 1 catch 2)" $ ExprTryCatch (ExprLitNum 1) (ExprLitNum 2)
    --test_parser "(try (raise 1) catch (lambda (x) (+ x 1)))" $ ExprRaise (ExprLitNum 2)
  ]

lispInterpret input expected cont = testCase input $ do
  result <- case cont of
    Just _ -> runCPSInterpreter input (ContNormal return)
    Nothing -> runInterpreter input
  putStrLn $ show result
  expected @=? result

test_lisp_interpret_cps =
  testGroup "test_lisp_cps_specific" [
  lispInterpret "(try 1 catch (lambda (x) x))" (Right (LispInt 1)) (Just undefined),
  lispInterpret "(try (raise 1) catch (lambda (x) (+ x 100)))" (Right (LispInt 101)) (Just undefined)
  ]

test_lisp_interpret cont  =
  testGroup
    ("test_lisp_interpret" <> (if isJust cont then "_cont" else ""))
    [
      lispInterpret "1" (Right $ LispInt 1) cont,
      lispInterpret "(+ 1 2)" (Right $ LispInt 3) cont,
      lispInterpret "(+ 5 (+ 1 2))" (Right $ LispInt 8) cont,
      lispInterpret "(+ 5  3)" (Right $ LispInt 8) cont,
      lispInterpret "(+ (+ 5 1)  (+ 6 1) 6)" (Right $ LispInt 19) cont,
      lispInterpret "(+ (+ 5 10) (- 6 1) 6)" (Right $ LispInt 26) cont,
      lispInterpret "(== 1 1)" (Right $ LispInt 1) cont,
      lispInterpret "(<= 1 1)" (Right $ LispInt 1) cont,
      lispInterpret "(>= 1 1)" (Right $ LispInt 1) cont,
      lispInterpret "(> 1 1)" (Right $ LispInt 0) cont,
      lispInterpret "(< 1 1)" (Right $ LispInt 0) cont,
      lispInterpret "(if 0 (* 5 10) (+ 3 4))" (Right $ LispInt 7) cont,
      lispInterpret "(if (- 5 1) (* 5 10) (+ 3 4))" (Right $ LispInt 50) cont,
      lispInterpret "let a = 10 in (* 10 20)" (Right (LispInt 200)) cont,
      lispInterpret "let a = 10 in let b=(* a 2) in (* a b)" (Right (LispInt 200)) cont,
      -- test shadowing
      lispInterpret "let a = 10 in let a=(* a 2) in a" (Right (LispInt 20)) cont,
      -- Application
      lispInterpret "let x = (lambda (y) y) in (x 10)" (Right (LispInt 10)) cont,
      lispInterpret "let a = 100 in let x = (lambda (y) y) in let z = 20 in (x (x z))" (Right (LispInt 20)) cont,
      lispInterpret "let a = 100 in let x = (lambda (w y) (+ w y)) in let z = 20 in (x (x z 1) 10)" (Right (LispInt 31)) cont,
      lispInterpret "let c = 10 in let a = 20 in let z = (lambda (x y) (* x y)) in (z c a)" (Right (LispInt 200)) cont,
      lispInterpret "let c = 10 in let a = 20 in let z = (lambda (x y) (* x y)) in (z (z c a) (z c a))" (Right (LispInt 40000)) cont,
      lispInterpret "letrec f = (lambda (x) (* x 20)) and g = (lambda (y) (*2 (f y))) in (g 10)" (Right (LispInt 400)) cont
    ]

test_prog1 cont =
  let source =
        [s|let x = 5 in
        let x = 38 in
            let f = (lambda (y z) (* y (+ x z))) in
              let g = (lambda (u) (* u x)) in
                (f (g 3) 17)
      |]
   in lispInterpret source (Right (LispInt 6270)) cont

test_prog2 cont =
  let source =
        [s|let makemult = (lambda (maker x)
                             (if x (+ 4 (maker maker (- x 1))) 0)) in
            let times4 = (lambda (x) (makemult makemult x)) in
                (times4 5)
          |]
   in lispInterpret source (Right (LispInt 20)) cont

test_prog3 cont =
  let source =
        [s|let makemult = (lambda (maker x)
                             (if (== x 1) 1 (* x (maker maker (- x 1))))) in
            let times4 = (lambda (x) (makemult makemult x)) in
                (times4 5)
          |]
   in lispInterpret source (Right (LispInt 120)) cont

test_prog4 cont =
  let source =
        [s|letrec even = (lambda (x) (if (== x 0) 1 (odd (- x 1)))) and
                  odd  = (lambda (x) (if (== x 0) 0 (even (- x 1))))
           in (even 6)
         |]
   in lispInterpret source (Right (LispInt 1)) cont

test_prog5 cont =
  let source =
        [s|letrec even = (lambda (x) (if (== x 0) 1 (odd (- x 1)))) and
                  odd  = (lambda (x) (if (== x 0) 0 (even (- x 1))))
           in (even 1001)
         |]
   in lispInterpret source (Right (LispInt 0)) cont

test_prog6 cont =
  let source =
        [s|letrec fib = (lambda (n) (if (<= n 1) n (+ (fib (-n 1)) (fib (- n 2)))))
           in (fib 20)
         |]
   in lispInterpret source (Right (LispInt 6765)) cont

test_prints =
  [ test_print "a",
    test_print "(a y)",
    test_print "(lambda (x)\n  (a y))",
    test_print "(if (a y)\n    ((lambda (a)\n       (x a))\n      z)\n    ((lambda (x)\n       (c d))\n      z))",
    test_print "((lambda (x y z)\n  (a x))\n 1)",
    test_print "((lambda (x)\n  (+   a x f))\n 10)",
    test_print "(<= a b)",
    test_print "(&& a b)",
    test_print "(|| (lambda (x)\n  (if y\n      z\n      z)) (lambda (x y)\n  (+   1 2)))"
  ]

test_lisp_parsers = do
  testGroup "test_lisp_parsers" $
    test_exprs ++ test_prints

-- test_interpret = testGroup "test_lisp_interpret" $ test_lisp_interpreter

main = do
  let tests = [test_prog1,
           test_prog2,
           test_prog3,
           test_prog4,
           test_prog5,
           test_prog6
         ]

  let non_cont =  testGroup "list_interpret_prog" $  L.map (\f -> f Nothing) tests
  let with_cont =  testGroup "list_interpret_prog" $  L.map (\f -> f (Just undefined)) tests

  defaultMain $ testGroup "test_lisp" [
    test_lisp_parsers,
    test_lisp_interpret Nothing,
    test_lisp_interpret (Just id),
    non_cont,
    with_cont,
    test_lisp_interpret_cps
    ]

--defaultMain tests
