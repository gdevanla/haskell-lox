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


test1 = testCase "test" $ do
  1 @?= 1

main = do
  defaultMain $ testGroup "tokenizer_tests_example_1" [test1]
