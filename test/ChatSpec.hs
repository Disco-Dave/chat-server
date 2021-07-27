{-# LANGUAGE QuasiQuotes #-}

module ChatSpec (spec) where

import qualified Chat
import Chat.Config (Config (..), LoggingConfig (..), WebSocketsConfig (..))
import qualified Chat.Config as Config
import Chat.Messages
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Functor (($>))
import qualified Data.Text as Text
import qualified Data.UUID.V4 as UUID
import qualified Katip
import Network.Socket.Free (getFreePort)
import qualified Network.WebSockets as WebSockets
import System.Directory (doesFileExist, removeFile)
import System.IO.Silently (silence)
import Test.Hspec
import qualified UnliftIO.Async as Async
import Debug.Trace (traceIO)

withServer :: ((WebSockets.ClientApp a -> IO a) -> IO ()) -> IO ()
withServer action = do
  let start = do
        config <- Config.loadFromEnv
        let port = webSocketsPort $ configWebSockets config -- getFreePort
        traceIO $ show port
        handle <-
          let testConfig =
                let logging = (configLogging config){loggingEnvironment = "testing"}
                    webSockets = (configWebSockets config){webSocketsPort = port}
                 in Config (configWebSockets config) logging
           in Async.async (silence $ Chat.run testConfig)
        Async.link handle
        pure (port, handle)

  let cleanup (_, handle) = do
        Async.cancel handle
  --exists <- doesFileExist filePath
  --when exists $ removeFile filePath

  bracket start cleanup $ \(port, _) -> do
    traceIO $ show port
    let runClient = WebSockets.runClient "127.0.0.1" 9090 "/"
     in action runClient

spec :: Spec
spec =
  around withServer $
    it "tells the user their userid after joining" $ \runClient -> do
      userId1 <- runClient $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-user", roomId: "test-room" } |])
        WebSockets.receiveData conn

      userId1 `shouldSatisfy` (not . Text.null)

      --userId2 <- runClient $ \conn -> do
        --WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-user", roomId: "test-room" } |])
        --WebSockets.receiveData conn

      --userId2 `shouldSatisfy` (not . Text.null)

      --userId1 `shouldNotBe` userId2
