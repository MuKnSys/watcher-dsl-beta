{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dsl.NodeRunner where 
import Data.Aeson
import System.Process
import Prelude
import Control.Exception
import System.Exit 
import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)


jsonDir :: String
jsonDir = "/tmp/parseTS.json"


startNode :: String -> IO(Either String String)
startNode fileName  = do
  putStrLn "Running node process..."
  let com = "node ../node/parser.js " ++ fileName
  output <- try $ readCreateProcess (shell com) {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
  case output of
    Left (e :: SomeException) -> do
      let errMsg = "Error running Node.js process: " ++ color Red (show e)
      return $ Left errMsg
    Right output -> do
      writeFile jsonDir output
      putStrLn output
      return $ Right output


