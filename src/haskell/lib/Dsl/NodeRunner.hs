{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dsl.NodeRunner where 
import Data.Aeson
import System.Process
import Prelude
import Control.Exception
import System.Exit 


jsonDir :: String
jsonDir = "/tmp/parseTS.json"


startNode :: String -> IO ()
startNode fileName  = do
  putStrLn "Running node process..."
  let com = "node ../node/parser.js " ++ fileName
  output <- try $ readCreateProcess (shell com) {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
  case output of
    Left (e :: SomeException) -> do
      putStrLn $ "Error running Node.js process: " ++ show e
      exitSuccess
    Right output -> do
      writeFile jsonDir output
      putStrLn output
      putStrLn "Done."


