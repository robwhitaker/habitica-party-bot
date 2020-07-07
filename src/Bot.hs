module Bot where

import qualified Colog

import           Control.Concurrent (forkIO)

import           Discord

import           Types
import           WebServer          (runServer)

runBot :: Text -> Env -> IO ()
runBot discordToken Env {..} =
    void $ Discord.runDiscord botOpts
  where
    logDebug = Colog.usingLoggerT envLogger . Colog.logDebug
    logInfo = Colog.usingLoggerT envLogger . Colog.logInfo
    botOpts = def
        { discordToken = discordToken
        , discordOnStart =
            void . forkIO . runServer envPort envSystemMessagesChannelId envLogger
        , discordOnEnd = logInfo "Bot shutting down."
        , discordOnEvent = const (logDebug . show)
        }
