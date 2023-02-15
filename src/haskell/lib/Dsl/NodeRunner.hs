{-# LANGUAGE OverloadedStrings #-}
module Dsl.NodeRunner where 
import Data.Aeson
import System.Process
import Prelude


jsonDir :: String
jsonDir = "/tmp/parseTS.json"


startNode :: String -> IO ()
startNode fileName  = do
  putStrLn "Running node process..."
  let com = "node ../node/parser.js " ++ fileName
  output <- readCreateProcess (shell com ) {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
  writeFile jsonDir output
  putStrLn $ output
  putStrLn $ "Done."

