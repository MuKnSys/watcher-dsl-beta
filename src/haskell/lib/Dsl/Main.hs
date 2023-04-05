{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Dsl.Main (main) where 

import qualified Dsl.NodeRunner as N
import qualified Dsl.ParseNode as P
import qualified Dsl.ConfigGenerator as CG 
import Prelude
import System.Environment
import Control.Exception
import System.Exit
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified System.FilePath as FP

import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)

-- main :: IO ()
-- main = do
--   arg <- getArgs
--   let styleFail = (color Red :: String -> String) . style Bold
--   case arg of
--     (fileName : _ ) -> do 
--       putStrLn $ "Parsing " ++ show fileName
--       nodeRes <- N.startNode fileName
--       case nodeRes of
--         Left errMsg -> do
--           putStrLn (styleFail errMsg)
--           exitSuccess
--         Right output -> do
--           file <- P.parse
--           exitSuccess
--     _ -> putStrLn "unrecognized args"
testWatchersPath :: FilePath
testWatchersPath = "/home/pawel/Desktop/watcher-dsl-beta/data/directoryForTestRun/testWatchers"

testPathForTSAST :: FilePath
testPathForTSAST = "/home/pawel/Desktop/watcher-dsl-beta/data/directoryForTestRun/generetedTSJSON"

testPathForGeneratedWatchers :: FilePath
testPathForGeneratedWatchers = "/home/pawel/Desktop/watcher-dsl-beta/data/directoryForTestRun/generatedWatcherCode"




main :: IO ()
main = do
  arg <- getArgs
  let styleFail = (color Red :: String -> String) . style Bold
  case arg of
    ("parse" : filesDir : _ ) -> do
      let pathEl = FP.splitDirectories $ filesDir
          pathForTSAST  = FP.joinPath $ (init pathEl ++ ["tsAST"])
          pathForWatcher = FP.joinPath $ (init pathEl ++ ["generatedWatcher"])
      putStrLn $ "Parsing " ++ show filesDir
      nodeRes <- N.startNode filesDir pathForTSAST
      case nodeRes of
        Left errMsg -> do
          putStrLn (styleFail errMsg)
          exitSuccess
        Right output -> do
          let pth = output
          jsonDirr <- P.mapJsonDirectory pathForTSAST
          tsWatcher <- P.generateCompiledWatcher jsonDirr
          P.generateDir pathForWatcher tsWatcher
          exitSuccess
    ("test" : _ ) -> do 
      putStrLn $ "Parsing example directory"
      nodeRes <- N.startNode testWatchersPath testPathForTSAST
      case nodeRes of
        Left errMsg -> do
          putStrLn (styleFail errMsg)
          exitSuccess
        Right output -> do
          let pth = output
          jsonDirr <- P.mapJsonDirectory testPathForTSAST
          putStrLn $ show$ jsonDirr
          tsWatcher <- P.generateCompiledWatcher jsonDirr
          putStrLn $ show $ tsWatcher
          P.generateDir testPathForGeneratedWatchers tsWatcher
          exitSuccess
    ("test-watcher" : _) -> do
      CG.configGenerator 
    _ -> putStrLn "unrecognized args"
