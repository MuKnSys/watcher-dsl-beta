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

-- loadFileMock = do
--   (jsonFile :: String ) <- readFile jsonDirr
--   let containsClassDeclaration = T.isInfixOf (T.pack "ClassDeclaration") (T.pack  jsonFile)
--   case containsClassDeclaration of
--     True -> error  "Class declaration not allowed"
--     False -> do
--       renderWatcher
--       putStrLn $ "Render File Created"


parse :: IO()
parse = do
  let styleFailMessage = color Red . style Bold
      styleFailInfo = color Red
      styleOK = color Green . style Bold
      styleOKInfo = color Green
  (jsonFile :: String) <- readFile jsonDirr
  putStrLn $ jsonFile 
  let maybeWatcher = A.eitherDecode (BS.fromStrict $ BS.pack jsonFile) :: Either String Watcher
  case maybeWatcher of
    Left err -> do
      putStrLn (styleFailMessage "Watcher syntactic rule violated: ")
      putStrLn (styleFailInfo err)
    Right watcher -> do
      encodeWatcher encodePath watcher
      let encodedWatcher = AP.encodePretty watcher
      renderWatcher (BSS.unpack encodedWatcher)
      putStrLn (styleOKInfo (show $ (BSS.unpack encodedWatcher)))
      putStrLn (styleOK "Watcher code parsed without errors")


  
encodePath :: FilePath
encodePath = "/tmp/encodedFile.json"

encodeWatcher :: FilePath -> Watcher -> IO ()
encodeWatcher path watcher = do
  let encodedWatcher = A.encode watcher
  BSS.writeFile path encodedWatcher


--renderWatcher :: CompileState -> FilePath -> IO ()
-- renderWatcher watcher path
renderWatcher code = do
  writeFile watcherPath code
  return ()
