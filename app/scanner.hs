{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
import Import hiding (many, (<|>), try)
import System.Environment
import System.IO (print)
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

main :: IO ()
main = return ()
  -- a <- getArgs
  -- case a of
  --   [s] -> either print print $ parse myParser "" s
  --   _ -> error "please pass one argument with the string to parse"
