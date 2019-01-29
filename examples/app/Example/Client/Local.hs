{-# Language OverloadedStrings #-}
--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module Example.Client.Local (
  start
) where 

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Example.Client (Client)
import qualified Example.Client as Client
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=),(.:))
import qualified Data.Aeson.Types as Aeson
import           Control.Concurrent (forkIO,ThreadId)

start :: (Aeson.Value -> IO ()) -> (IO Aeson.Value) -> Client -> IO ThreadId
start send receive impl = forkIO loop where 
  handleMessage (Aeson.Object o) = do
    method <- o .: "method"
    (Aeson.Object ps) <- o .: "params" -- TODO . This may be omitted
    case method of
      "message" -> do
        from <- ps .: "from"
        content <- ps .: "content"
        id <- o .: "id"
        return $ do
          result <- (Client.message impl) from content
          send $ Aeson.object [
            "id" .= (id :: Int),
            "result" .= result
            ]
      invalid -> fail $ "method " ++ invalid ++ " does not exist"     
  handleMessage _ = return (return ())
  loop = do
    msg <- receive
    case Aeson.parse handleMessage msg of
      Aeson.Error err -> return ()
      Aeson.Success r -> r
    loop  