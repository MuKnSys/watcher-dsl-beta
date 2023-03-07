{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dsl.NodeRunner where 
import Data.Aeson
import System.Process
import Prelude
import Control.Exception
import System.Exit 
import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
import qualified Data.Text as T


jsonDir :: String
jsonDir = "/tmp/parseTS.json"


-- startNode :: String -> IO(Either String String)
-- startNode fileName  = do
--   putStrLn "Running node process..."
--   let com = "node ../node/parser.js " ++ fileName
--   output <- try $ readCreateProcess (shell com) {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
--   case output of
--     Left (e :: SomeException) -> do
--       let errMsg = "Error running Node.js process: " ++ color Red (show e)
--       return $ Left errMsg
--     Right output -> do
--       writeFile jsonDir output
--       putStrLn output
--       return $ Right output

testDir = "/home/pawel/Desktop/watcher-dsl-beta/data/copyOfDir"

startNode :: String -> IO(Either String String)
startNode dir  = do
  putStrLn "Running node process..."
  let com = "node ../node/parseDir.js "
  output <- try $ readCreateProcess (shell com) {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
  case output of
    Left (e :: SomeException) -> do
      let errMsg = "Error running Node.js process: " ++ color Red (show e)
      return $ Left errMsg
    Right output -> do
      putStrLn $ "Ast files created"
      return $ Right output


parseTS :: String -> IO String
parseTS content = do
  let com = "node ../node/parseToTs.js " ++ show content
  putStrLn $ "Code in the node process "
  putStrLn $ content
  code <- readCreateProcess (shell com) {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
  return code
  
  
