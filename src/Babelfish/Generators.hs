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
renderTemplate (Template (TypeOptions casing (CompiledTemplate sequences) mappings) outs) name options (Defs tps apis) = map output outs
  where output (CompiledTemplate p, CompiledTemplate t) = (DT.renderTemplate p context, DT.renderTemplate t context)
        output other = error $ show other
        tcase = case casing of
          Camel -> pack . camel . unpack
          Pascal -> pack . pascal . unpack
        context = object $ [                                  
            "name" .= smartName name,
            "type" .= map tpe tps,                    
            "options" .= options
          ]
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