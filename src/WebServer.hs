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

import           Servant                  ((:>), Application, Handler, JSON,
                                           NoContent, PostNoContent, ReqBody,
                                           Server)
import qualified Servant

import           Web.Habitica

type API
    = "party" :> "groupChatReceived"
        :> ReqBody '[JSON] WebhookMessage
        :> PostNoContent

server :: DiscordHandle -> ChannelId -> LogAction IO Message -> Server API
server handle chanId logger =
    groupChatReceived
  where
    logInfo = Colog.usingLoggerT logger . Colog.logInfo
    logError = Colog.usingLoggerT logger . Colog.logError

    groupChatReceived :: WebhookMessage -> Handler NoContent
    groupChatReceived (GroupChatReceived GCR {..}) = do
        let MessageChat {..} = gcrChat
        case mcSender of
            SystemMessage -> do
                liftIO $ do
                    logInfo $ "Received a system message: \"" <> mcText <> "\""
                    logInfo "Sending to Discord..."
                    result <- Discord.restCall handle $ Discord.CreateMessage chanId mcText
                    case result of
                        Right _  -> logInfo "Success!"
                        Left err -> logError $ "Err: " <> show err
            _ -> liftIO $ logInfo "Received a user message; ignoring"
        return Servant.NoContent
    groupChatReceived _ = do
        liftIO $ logInfo "Received non-groupChatReceived webhook; ignoring"
        return Servant.NoContent

serverApp ::  DiscordHandle -> ChannelId -> LogAction IO Message -> Application
serverApp handle chanId logger =
    Servant.serve (Proxy :: Proxy API) (server handle chanId logger)

runServer :: Int -> ChannelId -> LogAction IO Message -> DiscordHandle -> IO ()
runServer port chanId logger handle = do
    logInfo $ "Running message listener server on port " <> show port
    Warp.run port (serverApp handle chanId logger)
  where
    logInfo = Colog.usingLoggerT logger . Colog.logInfo
