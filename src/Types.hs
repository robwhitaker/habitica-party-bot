module Types where

import           Colog         (LogAction, Message)

import           Data.Aeson    (FromJSON, parseJSON, (.:))
import qualified Data.Aeson    as Aeson
import           Data.UUID     (UUID)

import           Discord.Types (ChannelId)

import           Web.Habitica  (HabiticaAuthHeaders)

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
    , configGeneralChannelId        :: ChannelId
    , configLogFile                 :: FilePath
    , configHabiticaUserId          :: UUID
    , configAppName                 :: Text
    }

instance FromJSON LocalConfig where
    parseJSON = Aeson.withObject "LocalConfig" $ \o ->
        LocalConfig
            <$> o .: "port"
            <*> o .: "systemMessagesChannelId"
            <*> o .: "generalChannelId"
            <*> o .: "logFile"
            <*> o .: "habiticaUserId"
            <*> o .: "appName"

data Env = Env
    { envPort                    :: Int
    , envSystemMessagesChannelId :: ChannelId
    , envGeneralChannelId        :: ChannelId
    , envLogger                  :: LogAction IO Message
    , envHabiticaAuth            :: HabiticaAuthHeaders
    }
