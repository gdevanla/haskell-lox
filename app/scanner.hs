{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

--import Options.Applicative.Simple

import Data.Char
import Data.Text as T
import Data.Text.Encoding
-- import Text.Parsec.String.Char
-- import Text.Parsec.String.Char (anyCh

import ExprInterpreter
import FunctionsAndTypesForParsing (parseWithEof, parseWithLeftOver, regularParse)
import Import hiding (many, try, (<|>))
import qualified Paths_haskell_lox
import RIO.ByteString as BS
import RIO.Partial (read)
import RIO.Process
import Run
import System.Environment
import System.IO (print)
import Text.Parsec
import Text.Parsec.Char as PC
import Text.Parsec.String as PS
import Text.Parsec.String.Combinator (many1)

main :: IO ()
main = runSimpleApp $ do
  args <- liftIO getArgs
  case args of
    filename : _ -> do
      contents <- decodeUtf8 <$> BS.readFile filename
      void $ liftIO $ runScript contents
    [] -> do
      void $ liftIO runScriptInteractive
      liftIO $ return ()
