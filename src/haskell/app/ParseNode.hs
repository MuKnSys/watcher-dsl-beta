{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson.Types  as AT 

data CompileError = CompileError String
data CompiledWatcher = CompiledWatcher String


jsonDirr :: FilePath
jsonDirr = "/tmp/parseTS.json"

watcherPath :: FilePath
watcherPath = "/tmp/watcher.mock"


loadFile :: IO (Either CompileError CompiledWatcher)
loadFile = do
  jsonFile <- readFile jsonDirr
  let parse = A.eitherDecode (BS.fromStrict $ BS.pack jsonFile) >>= AT.parseEither (\obj -> do
                             s <- obj A..: "body"
                             return s )
  case parse of
     Left err -> return $ Left (CompileError err)
     Right res -> return $ Right (CompiledWatcher res)



--renderWatcher :: CompileState -> FilePath -> IO ()
-- renderWatcher watcher path
renderWatcher = do
  writeFile watcherPath "TODO"
  putStrLn $ "Done"
