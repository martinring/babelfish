{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Yaml as Yaml
import System.Exit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath  
import Text.Casing 
import Data.String (fromString)
import Control.Monad (forM_,when)
import System.Directory

import Babelfish.Data
import Babelfish.Generators
import Babelfish.Project

data Args = Args {
  input :: FilePath,
  language :: String
}

parseArgs :: Parser Args 
parseArgs = Args 
  <$> strOption
      (  long "input"
      <> value "babelfish.yaml"
      <> short 'i'
      <> metavar "INPUT"
      <> help "The project file to process")
  <*> strOption
      (  long "lang"
      <> value "all"
      <> short 'l'
      <> metavar "LANGUAGE"
      <> help "The output target")

run :: Args -> IO ()
run (Args input language) = do
  project <- Yaml.decodeFileEither input
  case project of
    Left err -> print err
    Right (Project name schema configs) -> do
      schema <- Yaml.decodeFileEither schema
      case schema of
        Left err -> print err
        Right schema -> do
          let activeConfigs = getConfigs (Text.pack language) configs          
          when (null activeConfigs) $ putStrLn "no active configurations"
          forM_ activeConfigs $ \(Config cname path apis options) -> do
            gen <- generator path
            Text.putStrLn $ Text.concat ["* Generating '",cname,"' configuration *"]
            forM_ (gen name options schema) $ \(path,content) -> do
              putStrLn $ "  -> " ++ path
              let dir = takeDirectory path
              createDirectoryIfMissing True dir            
              Text.writeFile path content     

  --let name = pascal (takeBaseName input)
  --file <- Yaml.decodeFileEither input  
  --gen <- generator language
  --case file of
  --  Left err -> print err
  --  Right schema -> do
  --    forM_ (gen (fromString name) schema) $ \(path,content) -> do
  --      putStrLn $ "# " ++ path ++ " #"
  --      Text.putStrLn content

main :: IO ()
main = run =<< execParser opts
  where opts = info (parseArgs <**> helper)
             ( fullDesc              
             <> header "babelfish")
