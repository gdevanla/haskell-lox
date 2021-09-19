{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Scanner
import ExprParser
import Text.Parsec as P

test_parser input expected = testCase input $ do
  let result = P.parse loxExpr  "" $ fromRight [] (scanner input)
  expected @=? result

test_exprs = [
  test_parser "1>5<=8;" (Right (Binary (Binary (Number 1.0) Gt (Number 5.0)) Lte (Number 8.0))),
  test_parser "1+1/2>5<=8;" (Right (Binary
                           (Binary
                             (Binary (Number 1.0) Plus
                               (Binary (Number 1.0) Slash (Number 2.0)))
                             Gt (Number 5.0)) Lte (Number 8.0))),
  test_parser "\"test\";" $ Right (Literal "test"),
  test_parser "true" $ Right $ LoxBool True,
  test_parser "nil" $ Right LoxNil,
  test_parser "(1+2)/2;" $ Right (Binary
                                  (Binary (Number 1.0) Plus (Number 2.0))
                                  Slash
                                  (Number 2.0)),
  test_parser "a=b=c=10;" $ Right (Assignment "a" (Assignment "b" (Assignment "c" (Number 10.0))))
  ]


test_statement input expected =  testCase input $ do
  let lexer = scanner input
  -- putStr $ show lexer
  let result = P.parse loxProgram "" $ fromRight [] lexer
  expected @=? result


test_statements = [
  test_statement "true;false;1+2;print 100;var x=10;" $
    Right [DeclStatement $ StmtExpr (LoxBool True),
           DeclStatement $ StmtExpr (LoxBool False),
           DeclStatement $ StmtExpr (Binary (Number 1.0) Plus (Number 2.0)),
           DeclStatement $ StmtPrint (Number 100.0),
           DeclVar $ Decl "x" (Just $ Number 10.0)
           ],
  test_statement "var x=10;" $ Right [DeclVar (Decl "x" (Just (Number 10.0)))],
  test_statement "var x;var y = 10; var z=\"value of z\";" $
    Right [DeclVar (Decl "x" Nothing), DeclVar (Decl "y" (Just (Number 10.0))), DeclVar (Decl "z" (Just (Literal "value of z")))],
  test_statement "var x;x;" $
    Right [DeclVar (Decl "x" Nothing), DeclStatement (StmtExpr (Identifier "x"))],
  test_statement "var a;var b;print a+b;" $
    Right $ [DeclVar (Decl "a" Nothing), DeclVar (Decl "b" Nothing), DeclStatement (StmtPrint (Binary (Identifier "a") Plus (Identifier "b")))],

  -- test blocks
  test_statement "var b=100; {var a=10;}" $ Right [DeclVar (Decl "b" (Just (Number 100.0))), DeclStatement (StmtBlock [DeclVar (Decl "a" (Just (Number 10.0)))])],
  test_statement "var b=100; {var a=10;} var x=100;" $
    Right [DeclVar (Decl "b" (Just (Number 100.0))),
           DeclStatement (StmtBlock [DeclVar (Decl "a" (Just (Number 10.0)))]),
           DeclVar (Decl "x" (Just (Number 100.0)))],

  -- if conditions
  test_statement "if (x==1) print x; else print true;" $
    Right
      [DeclStatement (StmtIf (IfElse (Binary (Identifier "x") EqualEqual (Number 1.0)) (StmtPrint (Identifier "x")) (Just (StmtPrint (LoxBool True)))))],

  test_statement "if (x==1) {print x;} else {print true;}" $
  Right [DeclStatement (StmtIf (IfElse (Binary (Identifier "x") EqualEqual (Number 1.0)) (StmtBlock [DeclStatement (StmtPrint (Identifier "x"))]) (Just (StmtBlock [DeclStatement (StmtPrint (LoxBool True))]))))]

  ]

test_parsers = test_exprs ++ test_statements

main = do
  defaultMain $ testGroup "test_parsesr" test_parsers
  --defaultMain tests
