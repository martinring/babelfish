{-# LANGUAGE TemplateHaskell
           , LambdaCase
           , ScopedTypeVariables #-}

module Babelfish.Files where

import qualified Data.ByteString as BS
import Data.FileEmbed
import Control.Exception
import Control.Applicative ((<|>))

builtin :: [(FilePath, BS.ByteString)]
builtin = $(embedDir "data")

tryPaths :: [FilePath] -> IO (Maybe BS.ByteString)
tryPaths [] = return Nothing
tryPaths (x:xs) =   
  try (BS.readFile x) >>= \case
    Left (e :: IOException) -> tryPaths xs
    Right c -> return (Just c)

getFile :: FilePath -> FilePath -> IO (Maybe BS.ByteString)
getFile base path = do
  fromDisk <- tryPaths [
    base ++ path ]  
  return $ fromDisk <|> lookup path builtin
