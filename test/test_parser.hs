{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Scanner
import ExprParser
import Text.Parsec as P

test_parser input expected = testCase input $ do
  let result = P.parse equality "" $ fromRight [] (scanner input)
  expected @=? result

test_equality = [
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
                                  (Number 2.0))
  ]

main = do
  defaultMain $ testGroup "test_parser" test_equality
  --defaultMain tests
