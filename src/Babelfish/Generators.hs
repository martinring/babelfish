--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Babelfish.Generators (Generator,generator) where

import Babelfish.Data
import Babelfish.Templates

--import Data.FileEmbed
import System.FilePath  
import qualified Data.ByteString.Lazy as BS
import Data.Either (fromRight)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text,unpack,pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Aeson 
import Text.Casing
import qualified Text.DocTemplates as DT
import qualified Data.HashMap.Strict as Map

type Generator = Text -> (Map.HashMap Text Value) -> Defs -> [(FilePath,Text)]

--builtin :: [(String, Template)]
--builtin = map (\(a,b) -> (drop 1 $ takeExtension a, fromRight (error "Hallo") (compileTemplate (decodeUtf8 b)))) $(embedDir "templates")

smartName :: Text -> Value
smartName name = object [
    "raw" .= name,
    "pascal" .= pascal (unpack name),
    "camel" .= camel (unpack name)
  ]

generator :: FilePath -> IO Generator
generator lang = do
  templ <- loadTemplate lang
  return $ renderTemplate templ
   -- where --builtins = return . fmap (\template -> renderTemplate template . defs) . (flip lookup) builtin        

check name True = [name .= True]
check name False = []

renderTemplate :: Template -> Generator
renderTemplate 
  (Template 
    (TypeOptions casing (CompiledTemplate sequences) mappings perType touts) 
    (APIOptions aouts)) 
  name 
  options 
  (Defs tps apis) = 
    filter (not.null.fst) $ (concatMap toutput touts) ++ (concatMap aoutput aouts)
  where toutput (CompiledTemplate p, CompiledTemplate t) = zip (map (DT.renderTemplate p) context) (map (DT.renderTemplate t) context)
        toutput other = error $ show other
        aoutput (CompiledTemplate p, CompiledTemplate t) = zip (map (DT.renderTemplate p) apiContext) (map (DT.renderTemplate t) apiContext)
        tcase = case casing of
          Camel -> pack . camel . unpack
          Pascal -> pack . pascal . unpack
        apiContext = map (\((API aname ms)) -> object $ [            
            "name" .= smartName name,            
            "api" .= object [
              "name" .= smartName aname,
              "method" .= map method ms,
              "local" .= True,
              "remote" .= True
            ],            
            "options" .= options  
          ]) apis
        context = 
          if perType 
          then map (\tp -> object [
            "name" .= smartName name,
            "type" .= tpe tp,
            "options" .= options
           ]) tps
          else [ object $ [                   
            "name" .= smartName name,
            "type" .= map tpe tps,                    
            "options" .= options
           ] ]
        isEmptyRecord (Record _ cs) = null cs
        tpe (Simple r@(Record name _)) = object $ [            
            "name" .= smartName name,
            "plain" .= True,
            "constructor" .= [ctor r]
          ]
        tpe (SumType name cs) = object $ [            
            "name" .= smartName name,            
            "constructor" .= map ctor cs
          ] ++ check "enum" (all isEmptyRecord cs)
        ctor (Record name r) = object $ [
            "name" .= smartName name,
            "parameter" .= map param r
          ] ++ check "empty" (null r)
        param (Field name t) = object $ [
            "name" .= smartName name,
            "type" .= typeName t
          ]
        typeName (Seq t) = DT.renderTemplate sequences (object ["elem" .= typeName t])
        typeName (Named n) = Map.lookupDefault (tcase n) n mappings
        method (Method name parameters return) = object $ [
            "name" .= smartName name,
            "parameter" .= map param parameters,
            "return" .= fmap (typeName) return
          ]