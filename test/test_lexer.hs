{-# LANGUAGE OverloadedStrings#-}
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

testToken test_name token_str expected_tok = testCase test_name $ do
  let result = scanner token_str
  case result of
    Left x -> assertFailure $ show x
    Right (x : _) -> assertEqual token_str (tok x) expected_tok



testInvalidToken test_name token_str = testCase test_name $ do
  let result = scanner token_str
  case result of
    Left x -> return ()
    Right x -> assertFailure $ show x

testSingleCharToken = testToken "testSingleCharToken" "+" (OpTok Plus)

testDoubleCharToken = testToken "testDoubleCharToken"  "==" (OpTok Eq)

testKeywordToken = testToken "testKeywordToken" "class" Class

testScanDouble_1 = testToken "testScanDouble_1" "1121.1121;" (Number 1121.1121)
testScanDouble_2 = testToken "testScanDouble_2" "0.1121;" (Number 0.1121)

-- We don't like the Lexer doing this, but we will try handling these scenarios in the parser

testScanDouble_4 = testToken "testScanDouble_4" "1121." (Number 1121.0)


testScanDoubleInvalid_2 = testInvalidToken "testScanDouble_2" "1121."

testScanIdentifier = testToken "testScanIdentifier" "and_1" (Identifier "and_1")

-- invalid tokens

testSingleCharInvalidToken = testInvalidToken "testSingleCharInvalidToken" "%"

testDoubleCharInvalidToken = testInvalidToken "testDoubleCharInvalidToken" "%%"

testScanInvalidDOT = testInvalidToken "testScanInvalidDoubleDOT" ".1121"

testScanInvalidIdentifier_1 = testInvalidToken "testScanInvalidIdentifier_1" "1and"
testScanInvalidIdentifier_2 = testInvalidToken "testScanInvalidIdentifier_2" "1_and"

testComment_1 = testToken "testComment" "// this is a comment" (Comment " this is a comment")
testComment_2 = testToken "testComment" "// this is a comment\n" (Comment " this is a comment")

main = do
  defaultMain $ testGroup "tokenizer_tests_example_1" [
    testSingleCharToken,
    testDoubleCharToken,
    testKeywordToken,
    testScanDouble_1,
    testScanDouble_2,


    testScanDouble_4,
    testScanIdentifier,
    -- invalid tokens
    testSingleCharInvalidToken,
    testDoubleCharInvalidToken,
    testScanInvalidIdentifier_1,
    testScanInvalidIdentifier_2,
    testScanInvalidDOT,
    testComment_1,
    testComment_2
    ]
  --defaultMain tests
