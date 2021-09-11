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
    Right (x : _) -> assertEqual token_str (tokinfo_type x) expected_tok



testInvalidToken test_name token_str = testCase test_name $ do
  let result = scanner token_str
  case result of
    Left x -> return ()
    Right x -> assertFailure $ show x

testSingleCharToken = testToken "testSingleCharToken" "+" PLUS

testDoubleCharToken = testToken "testDoubleCharToken"  "==" EQUAL_EQUAL

testKeywordToken = testToken "testKeywordToken" "class" CLASS

testScanDouble_1 = testToken "testScanDouble_1" "1121.1121" (NUMBER 1121.1121)
testScanDouble_2 = testToken "testScanDouble_2" "0.1121" (NUMBER 0.1121)

-- We don't like the Lexer doing this, but we will try handling these scenarios in the parser
testScanDouble_3_bad = testToken "testScanDouble_3" ".1121" (DOT)
testScanDouble_4_bad = testToken "testScanDouble_4" "1121.0" (NUMBER 1121.0)
testScanInvalidIdentifier_bad = testToken "testScanIdentifier" "1and" (NUMBER 1.0)

testScanDoubleInvalid_2 = testInvalidToken "testScanDouble_2" "1121."

testScanIdentifier = testToken "testScanIdentifier" "and_1" (IDENTIFIER "and_1")

-- invalid tokens

testSingleCharInvalidToken = testInvalidToken "testSingleCharInvalidToken" "%"

testDoubleCharInvalidToken = testInvalidToken "testDoubleCharInvalidToken" "%%"




main = do
  defaultMain $ testGroup "tokenizer_tests_example_1" [
    testSingleCharToken,
    testDoubleCharToken,
    testKeywordToken,
    testScanDouble_1,
    testScanDouble_2,
    testScanDouble_3_bad,
    testScanDouble_4_bad,
    testScanIdentifier,
    -- invalid tokens
    testSingleCharInvalidToken,
    testDoubleCharInvalidToken,
    testScanInvalidIdentifier_bad
    ]
  --defaultMain tests
