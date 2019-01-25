{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}

module Babelfish.Templates where

import qualified Text.DocTemplates as DT
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap.Strict as Map
import System.IO
import Control.Monad (forM)
import System.FilePath

instance Show DT.Template where
  show = const "<template>"

data DocTemplateRef = Inline Text
                    | Import FilePath
                    | CompiledTemplate DT.Template deriving Show

data Casing = Pascal | Camel

data TypeOptions = TypeOptions {
    typeCasing :: Casing,
    typeSequences :: DocTemplateRef,
    typeMappings :: Map.HashMap Text Text
  }

data Template = Template {
    typeOptions :: TypeOptions,
    outputs :: [(DocTemplateRef,DocTemplateRef)]        
  }

instance FromJSON Casing where
  parseJSON = withText "Casing" $ \case
    "pascal" -> return Pascal
    "camel" -> return Camel
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

instance FromJSON Template where
  parseJSON = withObject "Template File" $ \o -> Template 
    <$> o .: "types"
    <*> fmap (map (\(k,v) -> (Inline k,v)) .  Map.toList) (o .: "output")

deref :: FilePath -> DocTemplateRef -> IO DocTemplateRef
deref base (Import path) = do
  contents <- Text.readFile (base </> path)
  deref base (Inline contents)
deref _ (Inline text) =
  case DT.compileTemplate text of
    Left err -> error err
    Right templ -> return $ CompiledTemplate templ
deref _ other = return other

derefTemplates :: FilePath -> Template -> IO Template
derefTemplates base (Template (TypeOptions c seqs mappings) (outputs)) = do
  seqs' <- deref base seqs
  outputs' <- forM outputs $ \(p,r) -> do 
    p' <- deref base p
    r' <- deref base r
    return (p',r')
  return $ Template (TypeOptions c seqs' mappings) outputs'

loadTemplate :: FilePath -> IO Template
loadTemplate path = do  
  templ <- eitherDecodeFileStrict path
  case templ of 
    Left err -> error err
    Right templ -> 
      derefTemplates (takeDirectory path) templ