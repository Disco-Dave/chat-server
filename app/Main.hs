module Main where

import qualified Chat.Config as Config
import qualified Chat

main :: IO ()
main = Config.loadFromEnv >>= Chat.run
