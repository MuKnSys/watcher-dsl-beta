module Dsl.Main (main) where 

import qualified Dsl.NodeRunner as N
import qualified Dsl.ParseNode as P
import Prelude

main :: IO ()
main = do
  putStrLn "Enter file name to parse: "
  fileName <- getLine
  putStrLn $ "Parsing " ++ show fileName
  N.startNode fileName
  file <- P.loadFileMock
  putStrLn ""
  -- case file of
  --   Left err -> putStrLn $ show err
  --   Right res -> putStrLn $ show res
  
