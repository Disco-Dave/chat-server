module Chat.Config (
  WebSocketsConfig (..),
  LoggingConfig (..),
  Config (..),
  loadFromEnv,
) where

import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Data.Aeson (ToJSON)
import qualified Env
import GHC.Generics (Generic)
import qualified Katip
import LoadEnv (loadEnv)

newtype WebSocketsConfig = WebSocketsConfig
  { webSocketsPort :: Int
  }
  deriving (Show, Eq, Generic)
instance ToJSON WebSocketsConfig

parseWebSocketsConfig :: Env.Parser Env.Error WebSocketsConfig
parseWebSocketsConfig =
  WebSocketsConfig
    <$> Env.var Env.auto "WEB_SOCKETS_PORT" (Env.help "Port for the web sockets port to run on")

data LoggingConfig = LoggingConfig
  { loggingFile :: FilePath
  , loggingSeverity :: Katip.Severity
  , loggingVerbosity :: Katip.Verbosity
  , loggingEnvironment :: Katip.Environment
  }
  deriving (Show, Eq, Generic)
instance ToJSON LoggingConfig

parseLoggingConfig :: Env.Parser Env.Error LoggingConfig
parseLoggingConfig =
  LoggingConfig
    <$> Env.var (Env.str <=< Env.nonempty) "LOGGING_FILE" (Env.help "File to write logs to")
    <*> Env.var Env.auto "LOGGING_SEVERITY" (Env.help "Minimum katip severity of logs to write")
    <*> Env.var Env.auto "LOGGING_VERBOSITY" (Env.help "Katip verbosity of the logs")
    <*> Env.var (Env.str <=< Env.nonempty) "LOGGING_ENVIRONMENT" (Env.help "The environment of the application, ie production or development")

data Config = Config
  { configWebSockets :: WebSocketsConfig
  , configLogging :: LoggingConfig
  }
  deriving (Show, Eq, Generic)
instance ToJSON Config

parse :: IO Config
parse =
  Env.parse (Env.header "Chat Server") . Env.prefixed "CHAT_SERVER_" $
    liftA2 Config parseWebSocketsConfig parseLoggingConfig

loadFromEnv :: IO Config
loadFromEnv = loadEnv *> parse
