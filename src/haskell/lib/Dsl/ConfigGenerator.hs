module Dsl.ConfigGenerator where

import Prelude 
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import qualified System.Process as SP 


data Contract = Contract
  { name :: String
  , path :: String
  , kind :: String
  } deriving (Show)



data Config = Config
  { coContracts :: [Contract]
  , coOutputFolder :: String
  , coMode :: String
  , coKind :: String
  , coPort :: Int
  , coFlatten :: Bool
  , coSubgraphPath :: Maybe String
  } deriving (Show)

instance Y.ToJSON Contract where
  toJSON (Contract coName coPath coKind) =
    Y.object [ "name" Y..= coName
             , "path" Y..= coPath
             , "kind" Y..= coKind
             ]

instance Y.ToJSON Config where
  toJSON (Config contracts outputFolder mode kind port flatten subgraphPath) = Y.object
    [ "contracts" Y..= contracts
    , "outputFolder" Y..= outputFolder
    , "mode" Y..= mode
    , "kind" Y..= kind
    , "port" Y..= port
    , "flatten" Y..= flatten
    , "subgraphPath" Y..= subgraphPath
    ]


exampleContract :: Contract
exampleContract = Contract
  { name = "ERC721"
  , path = "../../node_modules/@openzeppelin/contracts/token/ERC721/ERC721.sol"
  , kind = "ERC721"
  }

exampleConfig :: Config
exampleConfig = Config
  { coContracts = [exampleContract]
  , coOutputFolder = "../demo-erc721-watcherTEST"
  , coMode = "all"
  , coKind = "active"
  , coPort = 3009
  , coFlatten = True
  , coSubgraphPath = Nothing
  }

configGenerator :: IO ()
configGenerator =
  do
    let path = "/home/pawel/Desktop/watchers/watcher-ts/packages/codegen/config.yaml"
    yaml <- Y.encodeFile path exampleConfig
    putStrLn $ "done"


yarnRunner :: IO () --move other module
yarnRunner = do
  let path = "/home/pawel/Desktop/watchers/watcher-ts/packages/codegen"
      com = "yarn codegen --config-file " ++ path ++ "/config.yaml"
  yarnProcess <- SP.readCreateProcess (SP.shell com ) {SP.cwd = Just path }[]
  return ()

--yarn codegen --config-file ./config.yaml
