{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}

module Babelfish.Templates where

import qualified Text.DocTemplates as DT
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap.Strict as Map
import Data.Text.Encoding (decodeUtf8)
import System.IO
import Control.Monad (forM)
import System.FilePath
import Babelfish.Files


instance Show DT.Template where
  show = const "<template>"

data DocTemplateRef = Inline Text
                    | Import FilePath
                    | CompiledTemplate DT.Template deriving Show

data Casing = Pascal | Camel | Kebab | Snake | QuietSnake | ScreamingSnake | Raw

data TypeOptions = TypeOptions {
    typeCasing :: Casing,
    typeSequences :: DocTemplateRef,
    typeMappings :: Map.HashMap Text Text,
    typeFilesPerType :: Bool,
    typeFiles :: [(DocTemplateRef,DocTemplateRef)]
  }

data APIOptions = APIOptions {
    apiFiles :: [(DocTemplateRef,DocTemplateRef)]
  }

data Template = Template {
    typeOptions :: TypeOptions,
    apiOptions :: APIOptions
  }

instance FromJSON Casing where
  parseJSON = withText "Casing" $ \case
    "pascal" -> return Pascal
    "camel" -> return Camel
    "kebab" -> return Kebab
    "snake" -> return Snake
    "quiet-snake" -> return QuietSnake
    "screaming-snake" -> return ScreamingSnake    
    "raw" -> return Raw
    invalid -> fail $ "invalid casing"

instance FromJSON DocTemplateRef where
  parseJSON (Object o) = case Map.lookup "import" o of
    Just (String t) -> return $ Import (Text.unpack t)
    invalid -> fail "no valid import object"
  parseJSON (String t) = return $ Inline t

instance FromJSON TypeOptions where
  parseJSON = withObject "Type Options" $ \o -> TypeOptions 
    <$> o .: "casing"
    <*> o .: "sequences"
    <*> o .: "mappings"
    <*> o .:? "files-per-type" .!= False
    <*> fmap (map (\(k,v) -> (Inline k,v)) .  Map.toList) (o .: "files")

instance FromJSON APIOptions where
  parseJSON = withObject "Type Options" $ \o -> APIOptions 
    <$> fmap (map (\(k,v) -> (Inline k,v)) .  Map.toList) (o .: "files")
        
instance FromJSON Template where
  parseJSON = withObject "Template File" $ \o -> Template 
    <$> o .: "types"
    <*> o .: "api"

deref :: FilePath -> DocTemplateRef -> IO DocTemplateRef
deref base (Import path) = do
  contents <- getFile "./" (base </> path)  
  let bs = case contents of
        Nothing -> error $ "template file does not exist: " ++ (base </> path)  
        Just bs -> bs
  deref base (Inline (decodeUtf8 bs))
deref _ (Inline text) =
  case DT.compileTemplate text of
    Left err -> error err
    Right templ -> return $ CompiledTemplate templ
deref _ other = return other

derefTemplates :: FilePath -> Template -> IO Template
derefTemplates base (Template (TypeOptions c seqs mappings perfile tfiles) (APIOptions afiles)) = do
  seqs' <- deref base seqs
  tfiles' <- forM tfiles $ \(p,r) -> do 
    p' <- deref base p
    r' <- deref base r    
    return (p',r')
  afiles' <- forM afiles $ \(p,r) -> do 
    p' <- deref base p
    r' <- deref base r    
    return (p',r')    
  return $ Template (TypeOptions c seqs' mappings perfile tfiles') (APIOptions afiles')

loadTemplate :: FilePath -> IO Template
loadTemplate path = do  
  file <- getFile "./" path
  case file of
    Nothing ->        
      error $ "couldnt find template: " ++ path
    Just file ->       
      case eitherDecodeStrict file of 
        Left err -> do
          putStrLn $ "Error in template: " ++ path
          error err
        Right templ -> 
          derefTemplates (takeDirectory path) templ