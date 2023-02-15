{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsl.ParseNode where 

import Prelude 
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson.Types  as AT
import qualified Data.Aeson.Text as ATT
import qualified Data.Text as T

data CompileError = CompileError String deriving Show 
data CompiledWatcher = CompiledWatcher String  deriving Show 


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


loadFileMock = do
  (jsonFile :: String ) <- readFile jsonDirr
  let containsClassDeclaration = T.isInfixOf (T.pack "ClassDeclaration") (T.pack  jsonFile)
  case containsClassDeclaration of
    True -> error  "Class declaration not allowed"
    False -> do
      renderWatcher
      putStrLn $ "Render File Created"
  


--renderWatcher :: CompileState -> FilePath -> IO ()
-- renderWatcher watcher path
renderWatcher = do
  writeFile watcherPath "TODO"
  return ()
