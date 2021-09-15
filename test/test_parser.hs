{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import RIO
import System.IO
import qualified RIO.Text as T
import qualified Data.Text as T
import qualified Data.List as L
import Prelude (print, read)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA
import Data.Array

import System.FilePath
import System.Directory (listDirectory)

import Scanner
import Data.Either

import Data.Text as T
import ExprParser
import Text.Parsec as P

test_equality = testCase "test_equality" $ do
  let lox_tokens = scanner "1+2==5+8;"
  let result = P.parse equality "" $ fromRight [] lox_tokens
  let expected =  Right (Binary (Binary (Number 1.0) Plus (Number 2.0)) EqualEqual (Binary (Number 5.0) Plus (Number 8.0)))
  assertEqual "equality_test" result expected

main = do
  defaultMain $ testGroup "test_parser" [
    test_equality
    ]
  --defaultMain tests
