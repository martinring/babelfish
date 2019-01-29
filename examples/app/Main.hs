module Main where

import Example.Messages
import qualified Example.Server as Server
import qualified Example.Server.Local as Server
import qualified Example.Server.Remote as Server
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Data.Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString (toStrict)

import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS

-- * the static part of the application (html + css + js)

static = Static.staticApp (staticSettings)

staticSettings = (Static.defaultFileServerSettings "ui/static")
  -- Disable caching for development
  { Static.ssMaxAge = Static.NoMaxAge }

-- * websocket setup
socket pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  mq <- newChan
  mq' <- dupChan mq
  let receiveLocal = readChan (mq :: Chan Value)
  let receiveRemote = readChan mq'  
  let send = WS.sendTextData conn . encode
  remote <- Server.connect send receiveRemote 
  local <- Server.start send receiveLocal $ Server.Server {
      Server.register = \(Person name age) -> return (age >= 18),
      Server.chat = \msg -> do
        Server.chat remote (msg ++ " zurück!")
    }
  forkIO $ do
    res <- Server.register remote (Person "Martin" 21)
    print res     
  handleSocket conn mq

handleSocket :: WS.Connection -> Chan Value -> IO ()
handleSocket conn mq = do
  -- Empfange die Daten vom Client
  msg <- WS.receiveData conn  
  case decodeStrict msg of
    Nothing -> putStrLn "parse failure"
    Just v -> writeChan mq v  
  handleSocket conn mq

-- * main

-- | starts the application on port 8000
main = do  
  Warp.run 8000 $
    WS.websocketsOr WS.defaultConnectionOptions socket static
