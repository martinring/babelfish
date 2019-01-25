{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Babelfish.Data where

import Data.Aeson ((.:),(.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Control.Applicative ((<|>))

type Name = Text.Text

type Named a = [(Text.Text,a)]

data TypeRef = Seq TypeRef
             | Named Name deriving (Eq,Show)

data Field = Field Name TypeRef deriving (Eq,Show)

data Record = Record Name [Field] deriving (Eq,Show)

data SumType = 
    Simple Record
  | SumType Name [Record] deriving (Eq,Show)

data Method = Method Name [Field] (Maybe TypeRef) deriving (Eq,Show)

data API = API Name [Method] deriving (Eq,Show)

data Defs = Defs [SumType] [API] deriving (Eq,Show)

instance Aeson.FromJSON Field where
  parseJSON = Aeson.withObject "Field" $ \o -> case Map.toList o of
    [(name, t)] -> Field name <$> Aeson.parseJSON t
    invalid -> fail "Field definitions must be an object with a single field"

instance Aeson.FromJSON Record where
  parseJSON (Aeson.String name) = return $ Record name []  
  parseJSON (Aeson.Object o) = case Map.toList o of    
    [(name, Aeson.Null)] -> return $ Record name []
    [(name, Aeson.Array fs)] ->
      Record name <$> mapM Aeson.parseJSON (Vector.toList fs)
    invalid -> fail "Record definitions must be either a string or an object with a single field"
  parseJSON invalid = Aeson.typeMismatch "Record" invalid

--instance Aeson.FromJSON SumType where
--  --parseJSON (Aeson.Null) = return $ Simple (Record [])
--  parseJSON a = 
--  
--  (Aeson.parseJSON a <$> 
--  (return . SumType . Map.toList =<< Aeson.parseJSON a)
--            <|> (return . Simple =<< Aeson.parseJSON a)    
--            <|> (return . SumType . map (\x -> (x,Record [])) =<< Aeson.parseJSON a)

instance Aeson.FromJSON Method where
  parseJSON = Aeson.withObject "Method" $ \o -> case Map.toList o of
    [(name, Aeson.Object o)] -> do
      parameters <- o .: "parameters"
      returnType <- o .:? "return"
      return $ Method name parameters returnType
    _ -> fail "expected method definition"
      
instance Aeson.FromJSON Defs where
  parseJSON = Aeson.withObject "Defs" $ \o -> do
      types <- mapM parseSumType =<< Map.toList <$> o .: "types"    
      api <- mapM parseAPI =<< Map.toList <$> o .: "api"
      --options <- o .: "options"
      return $ Defs types api
    where parseSumType (name,Aeson.Null) = return $ Simple (Record name [])
          parseSumType (name,Aeson.Array cof) = 
                (SumType name <$> mapM Aeson.parseJSON (Vector.toList cof))
            <|> (Simple . Record name <$> mapM Aeson.parseJSON (Vector.toList cof))
          parseAPI (name, Aeson.Null) = return $ API name []
          parseAPI (name, Aeson.Array ms) = API name <$> mapM Aeson.parseJSON (Vector.toList ms)

{-instance Aeson.FromJSON TypeDef where
  parseJSON (Aeson.String s) = return $ Product s []
  parseJSON (Aeson.Object o) = do
    name <- o .: "name"
    fields <- o .:? "fields"    
    variants <- o .:? "variants"
    td <- case (fields, variants) of 
            (Nothing, Nothing) -> return $ Product name []
            (Just fields, Nothing) -> return $ Product name fields
            (Nothing, Just variants) -> return $ Sum name variants
            _ -> fail "fields and variants exclude each other"
    abstract <- o .:? "abstract"
    td <- case abstract of
      Just True -> return $ Abstract td
      _ -> return td
    extends <- o .:? "extends"
    case extends of
      Just es -> return $ Extension td es
      _ -> return td
    

instance Aeson.FromJSON Field where
  parseJSON = Aeson.withObject "Field" $ \o -> do
    name <- o .: "name"
    tp <- o .: "type"
    return $ Field name tp-}

instance Aeson.FromJSON TypeRef where
  parseJSON (Aeson.Array v) = case Vector.toList v of
    [ t ] -> return . Seq =<< Aeson.parseJSON t
    _ -> fail "what?"
  parseJSON (Aeson.String t) = return $ Named t
  parseJSON invalid = Aeson.typeMismatch "type name" invalid