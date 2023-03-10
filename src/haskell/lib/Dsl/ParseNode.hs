{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}


module Dsl.ParseNode where

import Prelude 
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSS

import qualified Data.Aeson.Types  as AT
import qualified Data.Text as T
import qualified Data.Aeson.Encode.Pretty as AP


import System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
import Dsl.ParserDefinition
import GHC.Generics
import System.Directory
import qualified System.FilePath as FP
import qualified Dsl.NodeRunner as N 


jsonDirr :: FilePath
jsonDirr = "/tmp/parseTS.json"

watcherPath :: FilePath
watcherPath = "/tmp/watcher.mock"


loadFile :: IO (Either CompileError CompiledWatcher)
loadFile = do
  (jsonFile :: String ) <- readFile jsonDirr
  putStrLn $ jsonFile 
  let parse = A.eitherDecode (BS.fromStrict $ BS.pack jsonFile)>>= AT.parseEither (\obj -> do
                             body <- obj A..: "body"
                             values <- body A..: "type"
                             return values )
  case parse of
     Left err -> return $ Left (CompileError err)
     Right res -> return $ Right (CompiledWatcher res)


exampleDirr :: FilePath
exampleDirr = "/home/pawel/Desktop/watcher-dsl-beta/data"


-- parse :: IO (Either String CompiledWatcher)
-- parse = do
--   let styleFailMessage = color Red . style Bold
--       styleFailInfo = color Red
--       styleOK = color Green . style Bold
--       styleOKInfo = color Green
--   (jsonFile :: String) <- readFile jsonDirr
--   putStrLn $ jsonFile
--   let maybeWatcher = A.eitherDecode (BS.fromStrict $ BS.pack jsonFile) :: Either String Watcher
--   case maybeWatcher of
--     Left err -> do
--       putStrLn (styleFailMessage "Watcher syntactic rule violated: ")
--       putStrLn (styleFailInfo err)
--       return (Left err)
--     Right watcher -> do
--       encodeWatcher encodePath watcher
--       let encodedWatcher = AP.encodePretty watcher
--       renderWatcher (BSS.unpack encodedWatcher)
--       putStrLn (styleOKInfo (show $ (BSS.unpack encodedWatcher)))
--       putStrLn (styleOK "Watcher code parsed without errors")
--       return (Right (CompiledWatcher watcher))



parse :: String -> IO (Either String PreCompiledWatcher)
parse astToParse  = do
  let styleFailMessage = color Red . style Bold
      styleFailInfo = color Red
      styleOK = color Green . style Bold
  let maybeWatcher = A.eitherDecode (BS.fromStrict $ BS.pack astToParse) :: Either String Watcher
  case maybeWatcher of
    Left err -> do
      putStrLn (styleFailMessage "Watcher syntactic rule violated: ")
      putStrLn (styleFailInfo err)
      return (Left err)
    Right watcher -> do
      putStrLn (styleOK "Watcher code parsed without errors")
      return (Right (PreCompiledWatcher watcher))



testPath :: FilePath
testPath = "/home/pawel/Desktop/watcher-dsl-beta/data/generatedJsonDirectory"

-- mapDirectory :: FilePath -> IO GeneratedWatcherCode
-- mapDirectory path = do
--   isDir <- (doesDirectoryExist path :: IO Bool)
--   if isDir
--     then do
--       contents <- (listDirectory path :: IO [FilePath])
--       subdirs <- mapM (\p -> mapDirectory (path FP.</> p)) contents
--       let dirName = DirName (FP.takeFileName path)
--       return (Directory dirName subdirs)
--     else do
--       let fileName = FileName (FP.takeFileName path)
--       compiledWatcher <- (parse :: IO (Either String CompiledWatcher))
--       case compiledWatcher of
--           Left str -> return (Err "error")
--           Right compiledWatcher -> do
--             return (File fileName compiledWatcher)

mapJsonDirectory :: FilePath -> IO TSast
mapJsonDirectory path = do
  isDir <- (doesDirectoryExist path :: IO Bool)
  if isDir
    then do
      contents <- (listDirectory path :: IO [FilePath])
      subdirs <- mapM (\p -> mapJsonDirectory (path FP.</> p)) contents
      let dirName = DirName (FP.takeFileName path)
      return (TSDirectory dirName subdirs)
    else do
      let fileName = FileName (FP.takeFileName path)
          fullPath = path
      filcon <- readFile path
      precompiledWatcher <- parse filcon
      case precompiledWatcher of
          Left str -> return (TSErr "error")
          Right precompiledWatcher -> do
            return (TSFile fileName precompiledWatcher)

generateCompiledWatcher :: TSast -> IO GeneratedWatcherCode
generateCompiledWatcher (TSDirectory dirName contents) = do 
  newContents <- mapM generateCompiledWatcher contents
  return (Directory dirName newContents)
generateCompiledWatcher (TSFile fileName (PreCompiledWatcher watcher)) = do
  let encodeWatcher = BSS.unpack $ A.encode $ watcher 
  return (File fileName (CompiledWatcher encodeWatcher))
generateCompiledWatcher (TSErr errorCode) = do
  return (Err errorCode)



generateDir :: FilePath -> GeneratedWatcherCode -> IO ()
generateDir parent (Directory (DirName name) contents) = do
    let dirPath = parent FP.</> name
    createDirectoryIfMissing True dirPath
    mapM_ (generateDir dirPath) contents
generateDir parent (File (FileName name) (CompiledWatcher contents)) = do
    let filePath = FP.replaceExtension (parent FP.</> name) "ts"
    putStrLn $ "Input: " ++  show contents
    watcher <- N.parseTS contents
    putStrLn $ "generated watcher code: " ++  show watcher
    writeFile filePath watcher
generateDir [] (Err e) = do
  putStrLn $ e
generateDir (_:_) (Err e) = do
  putStrLn $ show e




encodePath :: FilePath
encodePath = "/tmp/encodedFile.json"


--renderWatcher :: CompileState -> FilePath -> IO ()
-- renderWatcher watcher path
renderWatcher code = do
  writeFile watcherPath code
  return ()


-- renderTest :: FilePath -> IO ()
-- renderTest path = do
--   code <- mapDirectory path
--   case code of
--     Err e-> putStrLn e
--     code -> writeFile "/tmp/ditMap.txt" (show code)
