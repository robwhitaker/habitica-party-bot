{-# LANGUAGE BangPatterns #-}

module Main where

import           System.Environment (lookupEnv)

import           Data.Aeson         (eitherDecodeFileStrict')
import qualified Data.UUID          as UUID

import           Bot                (runBot)
import           Types
import           Utils.Logging      (runWithBackgroundLogger)

import           Web.Habitica

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

    -- Fetch the Habitica API key from the environment and immediately wrap it
    -- up in the opaque type for auth headers so there will never be an opportunity
    -- to accidentally log it or send it somewhere. We need the bang pattern here
    -- to force evaluation so we fail out immediately if the API key is missing.
    !authHeaders <- maybe (error "Missing HABITICA_API_KEY environment variable")
                         (\apiKey -> habiticaHeaders
                                        configHabiticaUserId
                                        apiKey
                                        (xClient configHabiticaUserId configAppName)
                         )
                <$> ((>>= UUID.fromString) <$> lookupEnv "HABITICA_API_KEY")
    !discordToken <- maybe (error "Missing HPARTYBOT_DISCORD_TOKEN environment variable") toText
                <$> lookupEnv "HPARTYBOT_DISCORD_TOKEN"
    runWithBackgroundLogger configLogFile $ \logger ->
        runBot
            discordToken
            (Env configPort configSystemMessagesChannelId configGeneralChannelId logger authHeaders)
