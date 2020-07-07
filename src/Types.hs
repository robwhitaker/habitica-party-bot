module Types where

import           Colog         (LogAction, Message)

import           Data.Aeson    (FromJSON, parseJSON, (.:))
import qualified Data.Aeson    as Aeson

import           Discord.Types (ChannelId)

data Config = Config
    { configDev  :: LocalConfig
    , configProd :: LocalConfig
    }

instance FromJSON Config where
    parseJSON = Aeson.withObject "Config" $ \o ->
        Config
            <$> o .: "dev"
            <*> o .: "prod"

data LocalConfig = LocalConfig
    { configPort                    :: Int
    , configSystemMessagesChannelId :: ChannelId
    , configLogFile                 :: FilePath
    }

instance FromJSON LocalConfig where
    parseJSON = Aeson.withObject "LocalConfig" $ \o ->
        LocalConfig
            <$> o .: "port"
            <*> o .: "systemMessagesChannelId"
            <*> o .: "logFile"

data Env = Env
    { envPort                    :: Int
    , envSystemMessagesChannelId :: ChannelId
    , envLogger                  :: LogAction IO Message
    }
