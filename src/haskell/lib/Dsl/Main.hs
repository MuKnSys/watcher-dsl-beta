{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Dsl.Main (main) where 

import qualified Dsl.NodeRunner as N
import qualified Dsl.ParseNode as P
import qualified Dsl.ConfigGenerator as CG
import qualified Dsl.ParserDefinition as PD
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

testNodePath :: FilePath
testNodePath = "/home/pawel/Desktop/watcher-dsl-beta/src/node"

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
      nodeRes <- N.startNode filesDir pathForTSAST testNodePath
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
      nodeRes <- N.startNode testWatchersPath testPathForTSAST testNodePath
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
    -- ("test-watcher" : _) -> do
    --   CG.configGenerator


    ("compile" : path : pathToLib : pathToNode : _ ) -> do 
      putStrLn $ "Parsing example directory"
      N.startNode path path pathToNode
      P.forJsonDir (\(PD.FileName fn) pcw -> 
                   case (FP.takeExtension fn) of
                     ".json" -> do
                       let file = FP.dropExtension fn
                           c = P.dslToWatcher path pcw file
                           co = CG.CodeGeneratorCofnig
                             { CG.tempFile = "/tmp/w.yaml"
                             , CG.generatorPath = pathToLib
                             }
                       case c of
                         Right c -> do
                           CG.configGenerator co c
                         Left er ->  putStrLn er

                     _ -> return ()
                      ) path
    _ -> putStrLn "unrecognized args"
