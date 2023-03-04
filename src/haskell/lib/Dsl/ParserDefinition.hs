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
data CompiledWatcher = CompiledWatcher Watcher  deriving Show 

data GeneratedWatcherCode = Directory DirName [ GeneratedWatcherCode ] | File FileName CompiledWatcher | Err String  deriving Show

data DirName = DirName String  deriving Show
data FileName = FileName String  deriving Show

data Watcher = Watcher
  { pType :: String
  , pBody :: Maybe [Body]
  } deriving (Generic, Show)

data Body
  = FunctionDeclaration
    { fType        :: PTypes
    , fId          :: Id
    , fGenerator   :: PTypes
    , fExpression  :: PTypes
    , fAsync       :: PTypes
    , fParams      :: [Param]
    , fBody        :: BlockStatement
    , fReturnType  :: TSTypeAnnotation
    }
  | ExpressionStatement
    { eExpression :: Expression
    }
  | VariableDeclaration
    { vdType :: PTypes
    , vdDeclarations :: [VariableDeclarator]
    }
  | EnumDeclaration
    { edType :: PTypes
    , edId :: Id
    , edMembers :: [EnumMember]
    , edRange :: [PTypes]
    }
  deriving (Show)

data EnumMember = EnumMember
  { emType :: PTypes
  , emId :: Id
  , emRange :: [PTypes]
  , emInitializer :: Maybe EnumInitializer 
  } deriving (Show)

data EnumInitializer = EnumInitializer
  { eiType :: PTypes
  , eiValue :: PTypes
  , eiRaw :: PTypes
  , eiRange :: [PTypes]
  } deriving (Show)

data Id = Id
  { idType :: PTypes
  , idName :: PTypes
  , idRange :: [PTypes]
  } deriving (Generic, Show)

data Param = Param
  { paramTypeAnnotation :: TSTypeAnnotation
  , paramType :: PTypes
  , paramName :: PTypes
  , paramRange :: [PTypes]
  } deriving (Generic, Show)

data BlockStatement = BlockStatement
  { bsType :: PTypes
  , bsBody :: [Statement]
  , bsRange :: [PTypes]
  } deriving (Generic, Show)

data Statement
  = ReturnStatement
    { rType :: PTypes
    , rArgument :: Expression
    , rRange :: [PTypes]
    }
  | IfStatement
    { ifType :: PTypes
    , ifTest :: Test
    , ifConsequent :: BlockStatement
    , ifAlternate ::BlockStatement
    , ifRange :: [PTypes]
    }deriving (Generic, Show)

data Test
  = Test
  { tType :: PTypes
  , tOperator :: PTypes
  , tLeft :: ELExpression
  , tRight :: ELExpression
  , tRange :: [PTypes]
  } deriving (Generic, Show)

data PTypes
  = PString String
  | PNumber Int
  | PBool Bool
  | PSymbol String
  | PVoid
  | PNull
  deriving (Show, Generic)

data Expression
  = CallExpression
    { ceCallee :: Expression
    , ceArguments :: Maybe [Expression]
    , ceOptional :: PTypes
    , ceRange :: [PTypes]
    }
  | Literal
    { lType :: PTypes
    , lValue :: PTypes
    , lRaw :: PTypes 
    , lRange :: [PTypes]
    }
  | TemplateLiteral
    { lQuasis :: [TemplateElement]
    , lExpressions :: [ELExpression]
    , lRange :: [PTypes]
    }
  | BinaryExpression
    { beType :: PTypes
    , beOperator :: PTypes
    , beLeft :: ELExpression
    , beRight :: ELExpression
    , beRange :: [PTypes]
    }
  | ArrayExpression
    { arElements :: [ELExpression]
    , arRange :: [PTypes]
    }
  | ObjectExpression
    { obProperties :: [Property]
    , obRange :: [PTypes]
    }
  | MemberExpression
    { meObject :: Maybe Expression
    , meProperty :: Maybe Expression
    , meComputed :: Maybe PTypes
    , meOptional :: Maybe PTypes
    , meType :: Maybe PTypes
    , meName :: Maybe PTypes
    , meRange :: [PTypes]
    }
  | DecObjectExpression
    { oeType :: PTypes
    , oeProperties :: [Property]
    }
  | Identifier
    { iType :: PTypes
    , iName :: PTypes
    , iRange :: [PTypes]
    , idTypeAnnotation :: Maybe TSTypeAnnotation
    }
  deriving (Generic, Show)

data VariableDeclarator = VariableDeclarator
  { vdrType :: PTypes
  , vdrId :: Expression
  , vdrInit :: Expression
  , vdrRange :: [PTypes]
  } deriving (Show)



data Property = Property
  { opType :: PTypes
  , opKey :: Expression
  , opValue :: Expression
  , opComputed :: PTypes
  , opMethod :: PTypes
  , opShorthand :: PTypes
  , opKind :: PTypes
  , opRange :: [PTypes]
  } deriving (Generic, Show)



data TemplateElement = TemplateElement
  { teType :: PTypes
  , teValue :: ElValue
  , teTail :: Maybe PTypes
  , teRange :: [PTypes]
  } deriving (Generic, Show)

data ElValue = ElValue
  { elRaw :: PTypes
  , elCooked :: PTypes
  } deriving (Generic, Show)

data ELExpression = ELExpression
  { elType :: PTypes
  , elValue :: Maybe PTypes
  , elName :: Maybe PTypes
  , eleRaw :: Maybe PTypes
  , elRange :: [PTypes]
  }  deriving (Generic, Show)



data TSTypeAnnotation = TSTypeAnnotation
  { tsType :: PTypes
  , tsRange :: [PTypes]
  , tsTypeAnnotation :: TypeAnnotation
  } deriving (Generic, Show)

-- data TypeAnnotation = TSStringKeyword | TSNumberKeyword deriving (Generic, Show)

-- Instances

instance AT.FromJSON PTypes where
  parseJSON (AT.String s) = return (PString (T.unpack s))
  parseJSON (AT.Number n) = return (PNumber (truncate n))
  parseJSON (AT.Bool b)   = return (PBool b)
  parseJSON _             = fail "Invalid pTypes"
  
instance AT.FromJSON Watcher where
  parseJSON = AT.withObject "Program" $ \v ->
    Watcher <$> v A..: "type"
            <*> v A..: "body"
    
instance AT.FromJSON EnumMember where
  parseJSON (AT.Object v) = EnumMember
    <$> v A..: "type"
    <*> v A..: "id"
    <*> v A..: "range"
    <*> v A..:? "initializer"
  parseJSON _ = fail "Invalid Enum Member"

instance AT.FromJSON EnumInitializer where
  parseJSON (AT.Object v) = EnumInitializer 
    <$> v A..: "type"
    <*> v A..: "value"
    <*> v A..: "raw"
    <*> v A..: "range"
  parseJSON _ = fail "Invalid Enum Initializer"

instance AT.FromJSON Body where
  parseJSON (AT.Object v) = do
    bodyType <- v A..: "type" :: AT.Parser T.Text 
    case bodyType of
      "TSEnumDeclaration" -> EnumDeclaration
                             <$> v A..: "type"
                             <*> v A..: "id"
                             <*> v A..: "members"
                             <*> v A..: "range"
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
      "ClassDeclaration" -> fail "class declaration not allowed" 
      _ -> fail $ T.unpack $ bodyType


instance AT.FromJSON Test where
  parseJSON (AT.Object v) = Test <$>
                            v A..: "type" <*>
                            v A..: "operator" <*>
                            v A..: "left" <*>
                            v A..: "right" <*>
                            v A..: "range"
  parseJSON _ = fail "Invalid Test Type"

instance AT.FromJSON VariableDeclarator where
  parseJSON (AT.Object v) = VariableDeclarator
                            <$> v A..: "type"
                            <*> v A..: "id"
                            <*> v A..: "init"
                            <*> v A..: "range"
  parseJSON _ = fail "Invalid Variable Declarator"



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
  parseJSON (AT.Object v) = do
    stmtType <- v A..: "type" :: AT.Parser T.Text
    case stmtType of
      "ReturnStatement" -> ReturnStatement <$>
                           v A..: "type" <*>
                           v A..: "argument" <*>
                           v A..: "range"

      "IfStatement" -> IfStatement <$>
                       v A..: "type" <*>
                       v A..: "test" <*>
                       v A..: "consequent" <*>
                       v A..: "alternate" <*>
                       v A..: "range"
                       
      _ -> fail "Unknown Statement type"

      
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
      "ObjectExpression" -> ObjectExpression <$>
                            v A..: "properties" <*>
                            v A..: "range"
      "ArrayExpression" -> ArrayExpression <$>
                            v A..: "elements" <*>
                            v A..: "range"
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
                           v A..:? "arguments" <*>
                           v A..: "optional" <*>
                           v A..: "range"
      "Literal" -> Literal <$>
                    v A..: "type" <*>
                    v A..: "value" <*>
                    v A..: "raw" <*>
                    v A..: "range"
      "MemberExpression" -> MemberExpression <$>
                            v A..:? "object" <*>
                            v A..:? "property" <*>
                            v A..:? "computed" <*>
                            v A..:? "optional" <*>
                            v A..:? "type" <*>
                            v A..:? "name" <*>
                            v A..: "range"
      "DecObjectExpression" -> DecObjectExpression
                               <$> v A..: "type"
                               <*> v A..: "properties"
      "Identifier" -> Identifier <$>
                      v A..: "type" <*>
                      v A..: "name" <*>
                      v A..: "range" <*>
                      v A..:? "TypeAnnotation"

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
                            <*> v A..:? "raw"
                            <*> v A..:? "name"
                            <*> v A..: "range"
  parseJSON _ = fail "Invalid Element Expression"


instance AT.FromJSON ElValue where
  parseJSON (AT.Object v) = ElValue
                            <$> v A..: "raw"
                            <*> v A..: "cooked"
  parseJSON _ = fail "Element Value"

instance AT.FromJSON TSTypeAnnotation where
  parseJSON (AT.Object v) = TSTypeAnnotation <$>
                            v A..: "type" <*>
                            v A..: "range" <*>
                            v A..: "typeAnnotation"
  parseJSON _ = fail "Invalid Type Annotation"

data TypeAnnotation
  = TSTypeAnnotationAnnotation TSTypeAnnotation
  | TSStringKeywordAnnotation String
  | TSNumberKeywordAnnotation String
  | TSBooleanKeywordAnnotation String
  | TSTypeReference String
  deriving (Generic, Show)


instance AT.FromJSON TypeAnnotation where
  parseJSON (AT.Object v) = do
    annotationType <- v A..: "type"
    case annotationType of
      "TSTypeAnnotation" -> TSTypeAnnotationAnnotation <$>
                              v A..: "typeAnnotation"
      "TSTypeReference" -> TSTypeReference <$>
                             v A..: "type"
      "TSStringKeyword" -> TSStringKeywordAnnotation <$>
                             v A..: "type"
      "TSNumberKeyword" -> TSNumberKeywordAnnotation <$>
                             v A..: "type"
      "TSBooleanKeyword" -> TSBooleanKeywordAnnotation <$>
                              v A..: "type"
              
      "TSAnyKeyword"  -> error "any type not allowed"
      "TSUndefinedKeyword" -> error "undefined type not allowed"

                              
      _ -> fail $ "Invalid type annotation: " ++ annotationType

--TOJSON

instance AT.ToJSON PTypes where
  toJSON (PString s) = AT.String (T.pack s)
  toJSON (PNumber n) = AT.Number (fromIntegral n)
  toJSON (PBool b)   = AT.Bool b

instance AT.ToJSON Watcher where
  toJSON (Watcher pType pBody) =
    AT.object [ "type" A..= pType
              , "body" A..= pBody
              ]

instance AT.ToJSON Body where
  toJSON (EnumDeclaration etype eid emembers erange) =
    AT.object [ "type" A..= ("TSEnumDeclaration" :: T.Text)
              , "id" A..= eid
              , "members" A..= emembers
              , "range" A..= erange
              ]
  toJSON (VariableDeclaration vtype vdeclarations) =
    AT.object [ "type" A..= ("VariableDeclaration" :: T.Text)
              , "declarations" A..= vdeclarations
              , "type" A..= vtype
              ]
  toJSON (FunctionDeclaration ftype fid fgen fexp fasyn fparams fbody frettype) =
    AT.object [ "type" A..= ("FunctionDeclaration" :: T.Text)
              , "id" A..= fid
              , "generator" A..= fgen
              , "expression" A..= fexp
              , "async" A..= fasyn
              , "params" A..= fparams
              , "body" A..= fbody
              , "returnType" A..= frettype
              , "type" A..= ftype
              ]
  toJSON (ExpressionStatement eexp) =
    AT.object [ "type" A..= ("ExpressionStatement" :: T.Text)
              , "expression" A..= eexp
              ]



instance A.ToJSON EnumMember where
  toJSON (EnumMember eType eId eRange eInitializer) =
    AT.object [ "type" A..= eType,
               "id" A..= eId,
               "range" A..= eRange,
               "initializer" A..= eInitializer
             ]

instance AT.ToJSON EnumInitializer where
  toJSON (EnumInitializer eiType eiValue eiRaw eiRange) =
    AT.object [ "type" A..= eiType
             , "value" A..= eiValue
             , "raw" A..= eiRaw
             , "range" A..= eiRange
             ]

instance AT.ToJSON Test where
  toJSON (Test tType tOperator tLeft tRight tRange) = 
    AT.object [ "type" A..= tType
              , "operator" A..= tOperator
              , "left" A..= tLeft
              , "right" A..= tRight
              , "range" A..= tRange
              ]

instance AT.ToJSON VariableDeclarator where
  toJSON (VariableDeclarator vdType vdId vdInit vdRange) =
    AT.object [ "type" A..= vdType
              , "id" A..= vdId
              , "init" A..= vdInit
              , "range" A..= vdRange
              ]

instance AT.ToJSON Id where
  toJSON (Id idType idName idRange) =
    AT.object [ "type" A..= idType
              , "name" A..= idName
              , "range" A..= idRange
              ]

instance AT.ToJSON Param where
  toJSON (Param typeAnnotation pType pName pRange) = 
    AT.object [ "typeAnnotation" A..= typeAnnotation
              , "type" A..= pType
              , "name" A..= pName
              , "range" A..= pRange
              ]
    
instance AT.ToJSON BlockStatement where
  toJSON (BlockStatement bsType bsBody bsRange) =
    AT.object [ "type" A..= bsType
              , "body" A..= bsBody
              , "range" A..= bsRange
              ]
  

instance AT.ToJSON Statement where
  toJSON (ReturnStatement rsType rsArg rsRange) =
    AT.object [ "type" A..= rsType
              , "argument" A..= rsArg
              , "range" A..= rsRange
              ]
  toJSON (IfStatement ifType ifTest ifConsequent ifAlternate ifRange) =
    AT.object [ "type" A..= ifType
              , "test" A..= ifTest
              , "consequent" A..= ifConsequent
              , "alternate" A..= ifAlternate
              , "range" A..= ifRange
              ]

instance AT.ToJSON Property where
  toJSON (Property propType propKey propValue propComputed propMethod propShorthand propKind propRange) =
    AT.object [ "type" A..= propType
              , "key" A..= propKey
              , "value" A..= propValue
              , "computed" A..= propComputed
              , "method" A..= propMethod
              , "shorthand" A..= propShorthand
              , "kind" A..= propKind
              , "range" A..= propRange
              ]

instance AT.ToJSON Expression where
  toJSON (ObjectExpression oeProps oeRange) = AT.object
    [ "type" A..= ("ObjectExpression" :: T.Text)
    , "properties" A..= oeProps
    , "range" A..= oeRange
    ]
  toJSON (ArrayExpression aeElems aeRange) = AT.object
    [ "type" A..= ("ArrayExpression" :: T.Text)
    , "elements" A..= aeElems
    , "range" A..= aeRange
    ]
  toJSON (BinaryExpression beType beOperator beLeft beRight beRange) = AT.object
    [ "type" A..= ( "BinaryExpression" :: T.Text)
    , "operator" A..= beOperator
    , "left" A..= beLeft
    , "right" A..= beRight
    , "range" A..= beRange
    ]
  toJSON (TemplateLiteral tlQuasis tlExpression tlRange) = AT.object
    [ "type" A..= ( "TemplateLiteral" :: T.Text)
    , "quasis" A..= tlQuasis
    , "expressions" A..= tlExpression
    , "range" A..= tlRange
    ]
  toJSON (CallExpression ceCallee ceArguments ceOptional ceRange) = AT.object
    [ "type" A..= ( "CallExpression" :: T.Text)
    , "callee" A..= ceCallee
    , "arguments" A..= ceArguments
    , "optional" A..= ceOptional
    , "range" A..= ceRange
    ]
  toJSON (Literal liType  liValue liRaw liRange) = AT.object
    [ "type" A..= ( "Literal" :: T.Text)
    , "value" A..= liValue
    , "raw" A..= liRaw
    , "range" A..= liRange
    ]
  toJSON (MemberExpression meObject meProp meComputed meOptional meType meName meRange) = AT.object
    [ "type" A..=( "MemberExpression" :: T.Text)
    , "object" A..= meObject
    , "property" A..= meProp
    , "computed" A..= meComputed
    , "optional" A..= meOptional
    , "type" A..= meType
    , "name" A..= meName
    , "range" A..= meRange
    ]
  toJSON (DecObjectExpression doeType doeProps) = AT.object
    [ "type" A..= ( "DecObjectExpression" :: T.Text)
    , "properties" A..= doeProps
    ]
  toJSON (Identifier ideType ideName ideRange ideTypeAnn) = AT.object $
    [ "type" A..=( "Identifier" :: T.Text)
    , "name" A..= ideName
    , "range" A..= ideRange
    , "typeAnnotation" A..= ideTypeAnn
    ]

instance AT.ToJSON TemplateElement where
  toJSON (TemplateElement teType teValue teTail teRange) =
    AT.object [ "type" A..= teType
              , "value" A..= teValue
              , "tail" A..= teTail
              , "range" A..= teRange
              ]
instance AT.ToJSON ELExpression where
  toJSON (ELExpression eleType eleValue eleRaw eleName eleRange) =
    AT.object [ "type" A..= eleType
              , "value" A..= eleValue
              , "raw" A..= eleRaw
              , "name" A..= eleName
              , "range" A..= eleRange
              ]
instance AT.ToJSON ElValue where
  toJSON (ElValue elvRaw elvCooked) =
    AT.object [ "raw" A..= elvRaw
              , "cooked" A..= elvCooked
              ]

instance AT.ToJSON TSTypeAnnotation where
  toJSON (TSTypeAnnotation tstType tstRange tstTypeAnnotation) =
    AT.object [ "type" A..= ("TSTypeAnnotation" :: T.Text)
              , "range" A..= tstRange
              , "typeAnnotation" A..= tstTypeAnnotation
              ]

instance AT.ToJSON TypeAnnotation where
  toJSON (TSTypeAnnotationAnnotation tsaTypeAnnotation) =
    AT.object [ "type" A..= AT.String "TSTypeAnnotation"
              , "typeAnnotation" A..= tsaTypeAnnotation
              ]
  toJSON (TSTypeReference tsrType) =
    AT.object [ "type" A..= tsrType ]
  toJSON (TSStringKeywordAnnotation tsString) =
    AT.object [ "type" A..= AT.String "TSStringKeyword" ]
  toJSON (TSNumberKeywordAnnotation tsNumber) =
    AT.object [ "type" A..= AT.String "TSNumberKeyword" ]
  toJSON (TSBooleanKeywordAnnotation tsBool) =
    AT.object [ "type" A..= AT.String "TSBooleanKeyword" ]
