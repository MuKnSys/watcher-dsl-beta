{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsl.ParseNode where 

import Prelude 
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson.Types  as AT
import qualified Data.Text as T
import GHC.Generics

data CompileError = CompileError String deriving Show 
data CompiledWatcher = CompiledWatcher String  deriving Show 

data Program = Program
  { pType :: String
  , pBody :: [Body]
  } deriving (Generic, Show)

data Body
  = FunctionDeclaration
    { fType        :: String
    , fId          :: Id
    , fGenerator   :: Bool
    , fExpression  :: Bool
    , fAsync       :: Bool
    , fParams      :: [Param]
    , fBody        :: BlockStatement
    , fReturnType  :: TSTypeAnnotation
    }
  | ExpressionStatement
    { eExpression :: Expression
    }
  deriving (Generic, Show)

data Id = Id
  { idType :: String
  , idName :: String
  , idRange :: [Int]
  } deriving (Generic, Show)

data Param = Param
  { paramTypeAnnotation :: TSTypeAnnotation
  , paramType :: String
  , paramName :: String
  , paramRange :: [Int]
  } deriving (Generic, Show)

data BlockStatement = BlockStatement
  { bsType :: String
  , bsBody :: [Statement]
  , bsRange :: [Int]
  } deriving (Generic, Show)

data Statement
  = ReturnStatement
    { rType :: String
    , rArgument :: Expression
    , rRange :: [Int]
    }
  deriving (Generic, Show)

data Expression
  = CallExpression
    { ceCallee :: MemberExpression
    , ceArguments :: [Expression]
    , ceOptional :: Bool
    , ceRange :: [Int]
    }
  | Literal
    { lType :: String
    , lValue :: String
    , lRaw :: String
    , lRange :: [Int]
    }
  deriving (Generic, Show)

data MemberExpression = MemberExpression
  { meObject :: Identifier
  , meProperty :: Identifier
  , meComputed :: Bool
  , meOptional :: Bool
  , meRange :: [Int]
  } deriving (Generic, Show)

data Identifier = Identifier
  { iType :: String
  , iName :: String
  , iRange :: [Int]
  } deriving (Generic, Show)

data TSTypeAnnotation = TSTypeAnnotation
  { tsType :: String
  , tsRange :: [Int]
  , tsTypeAnnotation :: TypeAnnotation
  } deriving (Generic, Show)

data TypeAnnotation = TSStringKeyword | TSNumberKeyword deriving (Generic, Show)


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
