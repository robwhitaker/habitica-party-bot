module Main where

import           System.Environment (lookupEnv)

import           Data.Aeson         (eitherDecodeFileStrict')

import           Bot                (runBot)

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
    discordToken <- maybe (error "Missing HPARTYBOT_DISCORD_TOKEN environment variable") toText
                <$> lookupEnv "HPARTYBOT_DISCORD_TOKEN"
    runBot discordToken config
