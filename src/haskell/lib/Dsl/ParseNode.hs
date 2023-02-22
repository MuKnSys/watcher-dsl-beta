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
import qualified Data.Aeson.Types  as AT
import qualified Data.Text as T


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
  (jsonFile :: String) <- readFile jsonDirr
  putStrLn $ jsonFile 
  let maybeWatcher = A.eitherDecode (BS.fromStrict $ BS.pack jsonFile) :: Either String Watcher
  let styleFailMessage = color Red . style Bold
  let styleFailInfo = color Red
  let styleOK = color Green . style Bold
  let styleOKInfo = color Green
  case maybeWatcher of
    Left err -> do
      putStrLn (styleFailMessage "Watcher syntactic rule violated: ")
      putStrLn (styleFailInfo err)
    Right watcher -> do
      putStrLn (styleOKInfo (show $ watcher))
      putStrLn (styleOK "Watcher code parsed without errors")


--renderWatcher :: CompileState -> FilePath -> IO ()
-- renderWatcher watcher path
renderWatcher = do
  writeFile watcherPath "TODO"
  return ()
