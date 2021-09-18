{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
import Import hiding (many, (<|>), try)
import System.Environment
import System.IO (print)
import RIO.ByteString as BS
import Data.Text.Encoding
import Run
import RIO.Process

--import Options.Applicative.Simple
import qualified Paths_haskell_lox

import Data.Text as T
import Data.Char
import FunctionsAndTypesForParsing (parseWithEof, parseWithLeftOver, regularParse)
import Text.Parsec.String as PS
import Text.Parsec.Char as PC
-- import Text.Parsec.String.Char
-- import Text.Parsec.String.Char (anyCh
import Text.Parsec.String.Combinator (many1)

import Text.Parsec
import RIO.Partial (read)
import ExprInterpreter

main :: IO ()
main = runSimpleApp $ do
  args <- liftIO getArgs
  case args of
    filename:_ -> do
      contents <- decodeUtf8 <$> BS.readFile filename
      void $ liftIO $ runScript contents
    [] -> liftIO $ print "Please provide path to a lox file"
