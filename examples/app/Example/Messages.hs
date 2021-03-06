{-# LANGUAGE OverloadedStrings #-}
--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module Example.Messages where

import Data.Aeson ((.:),(.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

data Person = 
  Person {
    name :: String,
    age :: Int
  }

data Severity = 
  Info |
  Warning |
  Error

data ListThing = 
  Empty |
  Cons {
    head :: Int,
    tail :: ListThing
  }

-- CODEC

instance Aeson.FromJSON Person where
  parseJSON = Aeson.withObject "person" $ \o -> Person
    <$> o .: "name"
    <*> o .: "age"

instance Aeson.ToJSON Person where
  toJSON (Person name age) = Aeson.object [
      "name" .= name,
      "age" .= age]

instance Aeson.FromJSON Severity where
  parseJSON (Aeson.String "info") = return Info      
  parseJSON (Aeson.String "warning") = return Warning      
  parseJSON (Aeson.String "error") = return Error      
  parseJSON invalid = Aeson.typeMismatch "severity" invalid

instance Aeson.ToJSON Severity where
  toJSON Info = Aeson.String "info"
  toJSON Warning = Aeson.String "warning"
  toJSON Error = Aeson.String "error"

instance Aeson.FromJSON ListThing where
  parseJSON (Aeson.String "empty") = return Empty      
  parseJSON (Aeson.Object o) = do
    tpe <- o .: "type"
    case tpe of
      "cons" -> Cons
        <$> o .: "head"
        <*> o .: "tail"         
      invalid -> fail $ "invalid list thing type: " ++ invalid
  parseJSON invalid = Aeson.typeMismatch "list thing" invalid

instance Aeson.ToJSON ListThing where
  toJSON Empty = Aeson.String "empty"
  toJSON (Cons head tail) = Aeson.object [
    "type" .= ("cons" :: Text.Text),
    "head" .= head,
    "tail" .= tail]
