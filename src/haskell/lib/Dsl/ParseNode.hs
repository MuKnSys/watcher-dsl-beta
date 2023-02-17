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
import GHC.Generics

data CompileError = CompileError String deriving Show 
data CompiledWatcher = CompiledWatcher String  deriving Show 

data Watcher = Watcher
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
    } deriving (Generic, Show)

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
  | TemplateLiteral
    { lQuasis :: [TemplateElement]
    , lExpressions :: [ELExpression]
    , lRange :: [Int]
      
    }deriving (Generic, Show)

-- data BinOpepartors =
  
  


data TemplateElement = TemplateElement
  { teType :: String
  , teValue :: ElValue
  , teTail :: Maybe Bool
  , teRange :: [Int]
  } deriving (Generic, Show)

data ElValue = ElValue
  { elRaw :: String
  , elCooked :: String
  } deriving (Generic, Show)

data ELExpression = ELExpression
  { elType :: String
  , elName :: String
  , elRange :: [Int]
  }  deriving (Generic, Show)

data MemberExpression = MemberExpression
  { meObject :: Maybe Identifier
  , meProperty :: Maybe Identifier
  , meComputed :: Maybe Bool
  , meOptional :: Maybe Bool
  , meType :: Maybe String
  , meName :: Maybe String
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

-- data TypeAnnotation = TSStringKeyword | TSNumberKeyword deriving (Generic, Show)

data TypeAnnotation
  = TSTypeAnnotationAnnotation TSTypeAnnotation
  | TSStringKeywordAnnotation String
  | TSNumberKeywordAnnotation String
  | TSBooleanKeywordAnnotation String
  deriving (Generic, Show)

-- Instances
instance AT.FromJSON Watcher where
  parseJSON = AT.withObject "Program" $ \v ->
    Watcher <$> v A..: "type"
            <*> v A..: "body"

instance AT.FromJSON Body where
  parseJSON (AT.Object v) = do
    bodyType <- v A..: "type" :: AT.Parser T.Text 
    case bodyType of
      "ClassDeclaration" -> error "Class declaration not allowed"
      "FunctionDeclaration" -> FunctionDeclaration
                                <$> v A..: "type"
                                <*> v A..: "id"
                                <*> v A..: "generator"
                                <*> v A..: "expression"
                                <*> v A..: "async"
                                <*> v A..: "params"
                                <*> v A..: "body"
                                <*> v A..: "returnType"
      "ExpressionStatement" -> ExpressionStatement
                                <$> v A..: "expression"
      _ -> error "Invalid type for Body"

instance AT.FromJSON Id where
    parseJSON = AT.withObject "Id" $ \v -> Id
        <$> v A..: "type"
        <*> v A..: "name"
        <*> v A..: "range"

instance AT.FromJSON Param where
  parseJSON = AT.withObject "Param" $ \v -> do
    typeAnnotation <- v A..: "typeAnnotation"
    typeStr <- v A..: "type"
    name <- v A..: "name"
    range <- v A..: "range"
    return $ Param typeAnnotation typeStr name range

instance AT.FromJSON BlockStatement where
  parseJSON (AT.Object v) =
    BlockStatement <$>
      v A..: "type" <*>
      v A..: "body" <*>
      v A..: "range"

instance AT.FromJSON Statement where
  parseJSON (AT.Object v) =
    ReturnStatement <$>
      v A..: "type" <*>
      v A..: "argument" <*>
      v A..: "range"

instance AT.FromJSON Expression where
  parseJSON (AT.Object v) = do
    exprType <- v A..: "type" :: AT.Parser T.Text
    case exprType of
      "BinaryExpression" -> error "HERE"
        -- BinaryExpression <$> 
      "TemplateLiteral" -> TemplateLiteral <$>
                            v A..: "quasis" <*>
                            v A..: "expressions" <*>
                            v A..: "range"
      "CallExpression" -> CallExpression <$>
                            v A..: "callee" <*>
                            v A..: "arguments" <*>
                            v A..: "optional" <*>
                            v A..: "range"
      "Literal" -> Literal <$>
                      v A..: "type" <*>
                      v A..: "value" <*>
                      v A..: "raw" <*>
                      v A..: "range"

instance AT.FromJSON TemplateElement where
  parseJSON (AT.Object v) = 
    TemplateElement <$>
    v A..: "type" <*>
    v A..: "value" <*>
    v A..:? "tail" <*>
    v A..: "range" 
    

instance AT.FromJSON ELExpression where
  parseJSON (AT.Object v) =
    ELExpression <$> v A..: "type"
                 <*> v A..: "name"
                 <*> v A..: "range"


instance AT.FromJSON ElValue where
  parseJSON (AT.Object v) =
    ElValue <$>
    v A..: "raw" <*>
    v A..: "cooked"


instance AT.FromJSON MemberExpression where
  parseJSON (AT.Object v) =
    MemberExpression <$>
      v A..:? "object" <*>
      v A..:? "property" <*>
      v A..:? "computed" <*>
      v A..:? "optional" <*>
      v A..:? "type" <*>
      v A..:? "name" <*>
      v A..: "range"

instance AT.FromJSON Identifier where
  parseJSON (AT.Object v) =
    Identifier <$>
      v A..: "type" <*>
      v A..: "name" <*>
      v A..: "range"

instance AT.FromJSON TSTypeAnnotation where
  parseJSON (AT.Object v) =
    TSTypeAnnotation <$>
      v A..: "type" <*>
      v A..: "range" <*>
      v A..: "typeAnnotation"

instance AT.FromJSON TypeAnnotation where
  parseJSON (AT.Object v) = do
    annotationType <- v A..: "type"
    case annotationType of
      "TSTypeAnnotation" -> TSTypeAnnotationAnnotation <$>
                              v A..: "typeAnnotation"
      "TSStringKeyword" -> TSStringKeywordAnnotation <$>
                             v A..: "type"
      "TSNumberKeyword" -> TSNumberKeywordAnnotation <$>
                             v A..: "type"
      "TSBooleanKeyword" -> TSBooleanKeywordAnnotation <$>
                              v A..: "type"
      _ -> error $ "Invalid type annotation: " ++ annotationType



-- ### 

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
  
-- parse :: IO ()
parse = do
  (jsonFile :: String ) <- readFile jsonDirr
  putStrLn $ jsonFile 
  let maybeProgram = A.eitherDecode (BS.fromStrict $ BS.pack jsonFile) :: Either String Watcher

  -- handle the result
  case maybeProgram of
    Left err -> putStrLn err  -- print the error message if the decoding failed
    Right program -> print program
  

--renderWatcher :: CompileState -> FilePath -> IO ()
-- renderWatcher watcher path
renderWatcher = do
  writeFile watcherPath "TODO"
  return ()
