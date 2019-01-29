{-# LANGUAGE OverloadedStrings #-}
module Babelfish.Project where

import Babelfish.Data

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types
import Data.List (find)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as Map


data Project = Project Name FilePath [Config] deriving Show

data Config = Compound Name [Name]
            | Config Name FilePath [APIConfig] (Map.HashMap Text Value) deriving Show

isCompoundConfig (Compound _ _) = True
isCompoundConfig _ = False

configName (Compound n _) = n
configName (Config n _ _ _) = n

data InterfaceType = Local | Remote | Both deriving Show

data APIConfig = APIConfig Name InterfaceType deriving Show

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> case Map.toList o of
    [(name, Object m)] -> do
      path <- m .:? "schema" .!= (Text.unpack name ++ ".yaml")
      configs <- m .: "configs"  
      Project name path <$> mapM parseConfig (Map.toList configs)
    invalid -> fail "Project definitions must be an object with a single field"
   where parseConfig (name,String c) = return $ Compound name [c]
         parseConfig (name,Array cs) = Compound name <$> mapM parseJSON (Vector.toList cs)
         parseConfig (name,Object o) = do
           template <- o .:? "template" .!= ("babelfish/" ++ Text.unpack name ++ ".yaml")
           apiconfigs <- o .: "api"
           apiconfigs <- mapM parseAPIConfigs (Map.toList apiconfigs)
           options <- o .:? "options" .!= Map.empty
           return $ Config name template apiconfigs options
         parseConfig (name,invalid) = typeMismatch "config" invalid
         parseAPIConfigs (name,tpe) = APIConfig name <$> parseJSON tpe

instance FromJSON InterfaceType where
  parseJSON v@(String t) = case t of
    "local" -> return Local
    "remote" -> return Remote
    "both" -> return Both
    invalid -> typeMismatch "interface type" v

loadProject :: String -> IO Project
loadProject path = do  
  templ <- eitherDecodeFileStrict path
  case templ of 
    Left err -> error err
    Right templ -> return templ

getConfigs :: Text -> [Config] -> [Config]
getConfigs name confs = case (find ((name ==) . configName) confs) of
  Nothing -> if name == "all" then filter (not . isCompoundConfig) confs else []
  Just (Compound n cs) -> concatMap (flip getConfigs confs) cs
  Just (conf) -> [conf]
  
--  "babelfish/messages.yaml":  
--    "Haskell":
--      template: "babelfish/templates/haskell.yaml"
--      api:
--        server: local
--        client: remote      
--      options:    
--        base: "app/"
--        module: [Example]
--    "Scala JS":
--      template: "babelfish/templates/scalajs.yaml"    
--      api:
--        server: remote
--        client: local
--      options:
--        base: "ui/src/main/scala/"
--        module: [org, example]
--    default: [Haskell, Scala JS]