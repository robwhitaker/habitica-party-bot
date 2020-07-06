{-# LANGUAGE RecordWildCards #-}

module Bot where

import           Control.Concurrent (forkIO)

import           Discord

import           Types
import           WebServer          (runServer)

runBot :: Text -> Config -> IO ()
runBot discordToken Config {..} =
    void $ Discord.runDiscord botOpts
  where
    botOpts = def
        { discordToken = discordToken
        , discordOnStart = void . forkIO . runServer configPort configSystemMessagesChannelId
        , discordOnEnd = putStrLn "Bot shutting down."
        }
