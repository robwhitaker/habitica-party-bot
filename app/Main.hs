module Main where

import           System.Environment (lookupEnv)

import           Data.Aeson         (eitherDecodeFileStrict')

import           Bot                (runBot)
import           Types
import           Utils.Logging      (runWithBackgroundLogger)

data Mode
    = Dev
    | Prod
  deriving (Show, Eq, Ord, Read)

main :: IO ()
main = do
    botEnv <- fromMaybe Dev . (>>= readMaybe) <$> lookupEnv "HPARTYBOT_ENV"
    putStrLn $ "Running bot in environment: " <> show botEnv
    config <- either (error . ("Error parsing config file: " <>) . toText) id
          <$> eitherDecodeFileStrict' (
                case botEnv of
                    Dev  -> "config.json"
                    Prod -> "/etc/habitica-party-bot/config.json"
              )
    let LocalConfig {..} =
            case botEnv of
                Dev  -> configDev config
                Prod -> configProd config
    discordToken <- maybe (error "Missing HPARTYBOT_DISCORD_TOKEN environment variable") toText
                <$> lookupEnv "HPARTYBOT_DISCORD_TOKEN"
    runWithBackgroundLogger configLogFile $ \logger ->
        runBot discordToken (Env configPort configSystemMessagesChannelId logger)
