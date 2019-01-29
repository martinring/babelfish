{-# Language OverloadedStrings #-}
--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module $for(options.module)$$options.module$.$endfor$$api.name.pascal$.Local (
  start
) where 

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           $for(options.module)$$options.module$.$endfor$$api.name.pascal$ ($api.name.pascal$)
import qualified $for(options.module)$$options.module$.$endfor$$api.name.pascal$ as $api.name.pascal$
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=),(.:))
import qualified Data.Aeson.Types as Aeson
import           Control.Concurrent (forkIO,ThreadId)

start :: (Aeson.Value -> IO ()) -> (IO Aeson.Value) -> $api.name.pascal$ -> IO ThreadId
start send receive impl = forkIO loop where 
  handleMessage (Aeson.Object o) = do
    method <- o .: "method"
    (Aeson.Object ps) <- o .: "params" -- TODO . This may be omitted
    case method of
$for(api.method)$
      "$api.method.name.raw$" -> do
$for(api.method.parameter)$
        $api.method.parameter.name.camel$ <- ps .: "$api.method.parameter.name.raw$"
$endfor$
$if(api.method.return)$
        id <- o .: "id"
        return $$ do
          result <- ($api.name.pascal$.$api.method.name.camel$ impl) $for(api.method.parameter)$$api.method.parameter.name.camel$$sep$ $endfor$
          send $$ Aeson.object [
            "id" .= (id :: Int),
            "result" .= result
            ]
$else$
        return $$ ($api.name.pascal$.$api.method.name.camel$ impl) $for(api.method.parameter)$$api.method.parameter.name.camel$$sep$ $endfor$
$endif$         
$endfor$
      invalid -> fail $$ "method " ++ invalid ++ " does not exist"     
  handleMessage _ = return (return ())
  loop = do
    msg <- receive
    case Aeson.parse handleMessage msg of
      Aeson.Error err -> return ()
      Aeson.Success r -> r
    loop  