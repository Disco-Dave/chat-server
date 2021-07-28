{-# LANGUAGE QuasiQuotes #-}

module ChatSpec (spec) where

import qualified Chat
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Text as Text
import qualified Katip
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets
import Test.Hspec

withServer :: ((WebSockets.ClientApp a -> IO a) -> IO ()) -> IO ()
withServer action = do
  env <- Katip.initLogEnv "Chat" "testing"
  let app = Katip.runKatipContextT env () "chat-spec" Chat.makeApplication
  Warp.withApplication app $ \port ->
    action $ WebSockets.runClient "127.0.0.1" port "/"

spec :: Spec
spec =
  around withServer $ do
    it "tells the user their userid after joining" $ \runClient -> do
      userId1 <- runClient $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-user", roomId: "test-room" } |])
        WebSockets.receiveData conn

      userId1 `shouldSatisfy` (not . Text.null)

      userId2 <- runClient $ \conn -> do
        WebSockets.sendTextData conn (Aeson.encode [aesonQQ| { userName: "test-other-user", roomId: "test-room" } |])
        WebSockets.receiveData conn

      userId2 `shouldSatisfy` (not . Text.null)

      userId1 `shouldNotBe` userId2
