{-# LANGUAGE DeriveGeneric #-}
module Dsl.ParserDefinition where

import Prelude
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson.Types  as AT
import qualified Data.Text as T
import GHC.Generics

import Control.Applicative


data CompileError = CompileError String deriving Show 
data CompiledWatcher = CompiledWatcher String  deriving Show 

data Watcher = Watcher
  { pType :: String
  , pBody :: Maybe [Body]
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
  | VariableDeclaration
    { vdType :: String
    , vdDeclarations :: [VariableDeclarator]
    }
  deriving (Show)

data NoValue = NoValue
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
    , lValue :: Either Bool Int
    , lRaw :: Either String Int -- TODO Fix this 
    , lRange :: [Int]
    }
  | TemplateLiteral
    { lQuasis :: [TemplateElement]
    , lExpressions :: [ELExpression]
    , lRange :: [Int]
    }
  | BinaryExpression
    { beType :: String
    , beOperator :: String
    , beLeft :: ELExpression
    , beRight :: ELExpression
    , beRange :: [Int]
    }  deriving (Generic, Show)

-- data Value
--   = Object AT.Object
--   | Array AT.Array
--   | String T.Text
--   | Number Scientific
--   | Bool Bool
--   | Null
--   deriving (Generic, Show)
 



data VariableDeclarator = VariableDeclarator
  { vdrType :: String
  , vdrId :: Identifier
  , vdrInit :: ObjectExpression
  } deriving (Show)

data ObjectExpression = ObjectExpression
  { oeType :: String
  , oeProperties :: [Property]
  } deriving (Generic, Show)

data Property = Property
  { opType :: String
  , opKey :: Identifier
  , opValue :: Expression
  , opComputed :: Bool
  , opMethod :: Bool
  , opShorthand :: Bool
  , opKind :: String
  , opRange :: [Int]
  } deriving (Generic, Show)



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
  , elValue :: Maybe Int
  , elName :: Maybe String
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
  , idTypeAnnotation :: Maybe TSTypeAnnotation
  } deriving (Generic, Show)

data TSTypeAnnotation = TSTypeAnnotation
  { tsType :: String
  , tsRange :: [Int]
  , tsTypeAnnotation :: TypeAnnotation
  } deriving (Generic, Show)

-- data TypeAnnotation = TSStringKeyword | TSNumberKeyword deriving (Generic, Show)

-- Instances
instance AT.FromJSON Watcher where
  parseJSON = AT.withObject "Program" $ \v ->
    Watcher <$> v A..: "type"
            <*> v A..: "body"
    


instance AT.FromJSON Body where
  parseJSON (AT.Object v) = do
    bodyType <- v A..: "type" :: AT.Parser T.Text 
    case bodyType of
      "VariableDeclaration" ->  VariableDeclaration
                                <$> v A..: "type"
                                <*> v A..: "declarations"
      "FunctionDeclaration" ->  FunctionDeclaration
                                <$> v A..: "type"
                                <*> v A..: "id"
                                <*> v A..: "generator"
                                <*> v A..: "expression"
                                <*> v A..: "async"
                                <*> v A..: "params"
                                <*> v A..: "body"
                                <*> v A..: "returnType"
      "ExpressionStatement" ->  ExpressionStatement
                                <$> v A..: "expression"
      "ClassDeclaration" -> empty 
      --"ClassDeclaration" -> fail "class declaration not allowed" -- TODO refactor to not stop evaluation 
      _ -> fail $ T.unpack $ bodyType




instance AT.FromJSON VariableDeclarator where
  parseJSON (AT.Object v) = VariableDeclarator
    <$> v A..: "type"
    <*> v A..: "id"
    <*> v A..: "init"
  parseJSON _ = fail "Invalid Variable Declarator"

instance AT.FromJSON ObjectExpression where
  parseJSON (AT.Object v) = ObjectExpression
    <$> v A..: "type"
    <*> v A..: "properties"
  parseJSON _ = fail "Invalid Object Expression"


instance AT.FromJSON Id where
  parseJSON (AT.Object v) = Id
      <$> v A..: "type"
      <*> v A..: "name"
      <*> v A..: "range"
  parseJSON _ = fail "Invalid Id"

instance AT.FromJSON Param where
  parseJSON (AT.Object v) = Param <$>
      v A..: "typeAnnotation" <*>
      v A..: "type" <*>
      v A..: "name" <*>
      v A..: "range"
  parseJSON _ = fail "Invalid Parameter"

instance AT.FromJSON BlockStatement where
  parseJSON (AT.Object v) = BlockStatement <$>
      v A..: "type" <*>
      v A..: "body" <*>
      v A..: "range"
  parseJSON _ = fail "Invalid Block Statement"

instance AT.FromJSON Statement where
  parseJSON (AT.Object v) = ReturnStatement <$>
      v A..: "type" <*>
      v A..: "argument" <*>
      v A..: "range"
  parseJSON _ = fail "Invalid Return Statement"

instance AT.FromJSON Property where
  parseJSON (AT.Object v) = Property
    <$> v A..: "type"
    <*> v A..: "key"
    <*> v A..: "value"
    <*> v A..: "computed"
    <*> v A..: "method"
    <*> v A..: "shorthand"
    <*> v A..: "kind"
    <*> v A..: "range"
  parseJSON _ = fail "Invalid Property"
  
instance AT.FromJSON Expression where
  parseJSON (AT.Object v) = do
    exprType <- v A..: "type" :: AT.Parser T.Text
    case exprType of
      "BinaryExpression" -> BinaryExpression <$>
                             v A..: "type"  <*>
                             v A..: "operator" <*>
                             v A..: "left" <*>
                             v A..: "right" <*>
                             v A..: "range"

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
                    (Right <$> v A..: "value" <|> Left <$> v A..: "value") <*>
                    (Right <$> v A..: "raw" <|> Left <$> v A..: "raw") <*>
                    v A..: "range"
      _ -> fail $ T.unpack $ exprType

instance AT.FromJSON TemplateElement where
  parseJSON (AT.Object v) = TemplateElement
                            <$> v A..: "type"
                            <*> v A..: "value"
                            <*> v A..:? "tail"
                            <*> v A..: "range"
  parseJSON _ = fail "Invalid Template Element"

instance AT.FromJSON ELExpression where
  parseJSON (AT.Object v) = ELExpression
                            <$> v A..: "type"
                            <*> v A..:? "value"
                            <*> v A..:? "name"
                            <*> v A..: "range"
  parseJSON _ = fail "Invalid Element Expression"


instance AT.FromJSON ElValue where
  parseJSON (AT.Object v) = ElValue
                            <$> v A..: "raw"
                            <*> v A..: "cooked"
  parseJSON _ = fail "Element Value"


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
  parseJSON _ = fail "Invalid Member Expression"

instance AT.FromJSON Identifier where
  parseJSON (AT.Object v) =
    Identifier <$>
      v A..: "type" <*>
      v A..: "name" <*>
      v A..: "range" <*>
      v A..:? "TypeAnnotation"
  parseJSON _ = fail "Invalid Identifier"

instance AT.FromJSON TSTypeAnnotation where
  parseJSON (AT.Object v) =
    TSTypeAnnotation <$>
      v A..: "type" <*>
      v A..: "range" <*>
      v A..: "typeAnnotation"
  parseJSON _ = fail "Invalid Type Annotation"

data TypeAnnotation
  = TSTypeAnnotationAnnotation TSTypeAnnotation
  | TSStringKeywordAnnotation String
  | TSNumberKeywordAnnotation String
  | TSBooleanKeywordAnnotation String
  deriving (Generic, Show)


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
      "TSAnyKeyword"  -> error "any type not allowed"
      "TSUndefinedKeyword" -> error "undefined type not allowed"

                              
      _ -> fail $ "Invalid type annotation: " ++ annotationType


options :: AT.Options
options = AT.defaultOptions { AT.omitNothingFields = True }
