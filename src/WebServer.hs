{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module WebServer where

import           Colog                    (LogAction, Message)
import qualified Colog                    as Colog

import           Discord                  (DiscordHandle)
import qualified Discord
import qualified Discord.Requests         as Discord (ChannelRequest (CreateMessage))
import           Discord.Types            (ChannelId)

import qualified Network.Wai.Handler.Warp as Warp

import           Data.Aeson               (Value)
import           Servant                  ((:<|>) (..), (:>), Application,
                                           Handler, JSON, NoContent,
                                           PostNoContent, ReqBody, Server)
import qualified Servant

import           Web.Habitica

type API
    = "party" :> "groupChatReceived"
        :> ReqBody '[JSON] WebhookMessage
        :> PostNoContent
--    :<|> "party" :> "questActivity"
--        :> ReqBody '[JSON] WebhookMessage
--        :> PostNoContent
    :<|> "party" :> "questActivity"
        :> ReqBody '[JSON] Value
        :> PostNoContent

server :: DiscordHandle -> ChannelId -> ChannelId -> LogAction IO Message -> Server API
server handle smChanId gChanId logger =
    groupChatReceived :<|> cheeseIt
  where
    logInfo = Colog.usingLoggerT logger . Colog.logInfo
    logError = Colog.usingLoggerT logger . Colog.logError

    cheeseIt :: Value -> Handler NoContent
    cheeseIt val = do
        liftIO $ logError $ show val
        return Servant.NoContent

    questActivity :: WebhookMessage -> Handler NoContent
    questActivity (QuestActivity QA {..}) = do
        void $ case qaType of
            QuestInvited -> do
                liftIO $ do
                    logInfo "Received a QuestInvited message"
                    logInfo "Notifying users on Discord..."
                    result <- Discord.restCall handle $ Discord.CreateMessage gChanId
                                "Hey @everyone - invites for the next quest have been sent out!"
                    case result of
                        Right _  -> logInfo "Success!"
                        Left err -> logError $ "Err: " <> show err
            _            -> liftIO $ logInfo "Not a quest invite; ignoring"
        return Servant.NoContent
    questActivity _ = do
        liftIO $ logInfo "Received unhandled questActivity webhook; ignoring"
        return Servant.NoContent

    groupChatReceived :: WebhookMessage -> Handler NoContent
    groupChatReceived (GroupChatReceived GCR {..}) = do
        let MessageChat {..} = gcrChat
        case mcSender of
            SystemMessage -> do
                liftIO $ do
                    logInfo $ "Received a system message: \"" <> mcText <> "\""
                    logInfo "Sending to Discord..."
                    result <- Discord.restCall handle $ Discord.CreateMessage smChanId mcText
                    case result of
                        Right _  -> logInfo "Success!"
                        Left err -> logError $ "Err: " <> show err
            _ -> liftIO $ logInfo "Received a user message; ignoring"
        return Servant.NoContent
    groupChatReceived _ = do
        liftIO $ logInfo "Received non-groupChatReceived webhook; ignoring"
        return Servant.NoContent

serverApp ::  DiscordHandle -> ChannelId -> ChannelId -> LogAction IO Message -> Application
serverApp handle smChanId gChanId logger =
    Servant.serve (Proxy :: Proxy API) (server handle smChanId gChanId logger)

runServer :: Int -> ChannelId -> ChannelId -> LogAction IO Message -> DiscordHandle -> IO ()
runServer port smChanId gChanId logger handle = do
    logInfo $ "Running message listener server on port " <> show port
    Warp.runSettings settings (serverApp handle smChanId gChanId logger)
  where
    logInfo = Colog.usingLoggerT logger . Colog.logInfo
    simpleLogger req status _ = do
        logInfo $ show req <> " " <> show status
    settings = Warp.setPort port $ Warp.setLogger simpleLogger Warp.defaultSettings
