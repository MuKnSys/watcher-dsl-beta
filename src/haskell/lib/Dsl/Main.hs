module Dsl.Main (main) where 

import qualified Dsl.NodeRunner as N
import qualified Dsl.ParseNode as P
import Prelude
import System.Environment

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    (fileName : _ ) -> do 
      putStrLn $ "Parsing " ++ show fileName
      N.startNode fileName
      file <- P.loadFileMock
      putStrLn ""
  -- case file of
  --   Left err -> putStrLn $ show err
  --   Right res -> putStrLn $ show res
    _ -> putStrLn "unrecognized args"
