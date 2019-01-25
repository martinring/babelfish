{-# LANGUAGE OverloadedStrings #-}
module Interface where

import Control.Concurrent.MVar
import Data.ByteString.Builder
import Data.Monoid
import Data.Foldable (foldMap)
import Data.List (intersperse)
import Data.Int (Int32)

data List =
  Empty | Cons { head :: Int32, tail :: List } deriving Show
data Person =
  Person { age :: Int, name :: String }
data Severity =
  Info | Warning | Error
data Point =
  Point { x :: Int, y :: Int }

renderList :: List -> Builder
renderList Empty      = word8 1
renderList (Cons h t) = word8 2 <> int32LE h <> (renderList t)

readList :: ByteString -> List