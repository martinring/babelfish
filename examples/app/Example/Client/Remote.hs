{-# Language OverloadedStrings #-}
--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module Example.Client.Remote (
  connect
) where 

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Example.Client (Client)
import qualified Example.Client as Client
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Control.Concurrent.MVar
import           Control.Concurrent (forkIO)
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=),(.:?))
import qualified Data.Aeson.Types as Aeson

connect :: (Aeson.Value -> IO ()) -> (IO Aeson.Value) -> IO Client
connect send receive = do  
  id <- newMVar (0 :: Int)
  let genId = modifyMVar id $ \id -> return (id + 1, id)
  handlers <- newMVar (Map.empty :: Map Int (MVar Aeson.Value))
  let message from content = do
        id' <- genId
        send $ Aeson.object [
          "jsonrpc" .= ("2.0" :: String),
          "method" .= ("message" :: String),
          "id" .= id',
          "params" .= Aeson.object [
            "from" .= from,  
            "content" .= content
            ]
          ]
        returnValue <- newEmptyMVar
        modifyMVar_ handlers $ \handlers -> 
          return (Map.insert id' returnValue handlers)
        json <- takeMVar returnValue
        case Aeson.parse (Aeson.parseJSON) json of
          Aeson.Error err -> fail "could not decode response"
          Aeson.Success x -> return x
  let handleResponse (Aeson.Object o) = do
        error <- o .:? "error"
        result <- o .:? "result"
        id <- o .:? "id"
        case (error,result,id) of
          (Just err, _, Just id) -> return $
            modifyMVar_ handlers $ \ handlers -> do
              let handler = Map.lookup id handlers
              case handler of
                Just handler -> putMVar handler err
                Nothing -> return ()
              return (Map.delete id handlers)
          (_, Just r, Just id) -> return $
            modifyMVar_ handlers $ \ handlers -> do
              let handler = Map.lookup id handlers
              case handler of
                Just handler -> putMVar handler r
                Nothing -> return ()
              return (Map.delete id handlers)
          (_, _, _) -> return $ return ()
      handleResponse _ = return $ return ()
  let loop = do
        msg <- receive
        case Aeson.parse handleResponse msg of
          Aeson.Error err -> return ()
          Aeson.Success x -> x
        loop  
  forkIO loop
  return $ Client.Client message