module NodeRunner where 
import Data.Aeson
import System.Process 


jsonDir :: String
jsonDir = "/tmp/parseTS.json"


startNode :: IO ()
startNode = do
  putStrLn "Running node process..."
  output <- readCreateProcess (shell "node ../node/parser.js") {cwd = Just "/home/pawel/Desktop/watcher-dsl-beta/src/node"} []
  writeFile jsonDir output 
  putStrLn $ "Done."

