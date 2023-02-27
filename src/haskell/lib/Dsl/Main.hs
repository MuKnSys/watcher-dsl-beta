{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Dsl.Main (main) where 

import qualified Dsl.NodeRunner as N
import qualified Dsl.ParseNode as P
import Prelude
import System.Environment
import Control.Exception
import System.Exit
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)

--main :: IO ()
main = do
  arg <- getArgs
  let styleFail = (color Red :: String -> String) . style Bold
  case arg of
    (fileName : _ ) -> do 
      putStrLn $ "Parsing " ++ show fileName
      nodeRes <- N.startNode fileName
      case nodeRes of
        Left errMsg -> do
          putStrLn (styleFail errMsg)
          exitSuccess
        Right output -> do
          file <- P.parse
          exitSuccess
    _ -> putStrLn "unrecognized args"
