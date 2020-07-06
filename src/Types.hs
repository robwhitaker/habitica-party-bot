module Types where

import           Data.Aeson    (FromJSON, parseJSON, (.:))
import qualified Data.Aeson    as Aeson

import           Discord.Types (ChannelId)

data Config = Config
    { configPort                    :: Int
    , configSystemMessagesChannelId :: ChannelId
    }

instance FromJSON Config where
    parseJSON = Aeson.withObject "Config" $ \o ->
        Config
            <$> o .: "port"
            <*> o .: "systemMessagesChannelId"
