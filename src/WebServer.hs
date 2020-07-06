{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module WebServer where

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

server :: DiscordHandle -> ChannelId -> Server API
server handle chanId =
    groupChatReceived
  where
    groupChatReceived :: WebhookMessage -> Handler NoContent
    groupChatReceived (GroupChatReceived GCR {..}) = do
        let MessageChat {..} = gcrChat
        case mcSender of
            SystemMessage -> do
                liftIO $ do
                    putStrLn "Received a system message; sending to Discord"
                    result <- Discord.restCall handle $ Discord.CreateMessage chanId mcText
                    case result of
                        Right _  -> putStrLn "Success!"
                        Left err -> putStrLn $ "Err: " <> show err
            _ -> liftIO $ putStrLn "Received a user message; ignoring"
        return Servant.NoContent
    groupChatReceived _ = do
        liftIO $ putStrLn "Received non-groupChatReceived webhook; ignoring"
        return Servant.NoContent

serverApp ::  DiscordHandle -> ChannelId -> Application
serverApp handle chanId =
    Servant.serve (Proxy :: Proxy API) (server handle chanId)

runServer :: Int -> ChannelId -> DiscordHandle -> IO ()
runServer port chanId handle = do
    putStrLn $ "Running message listener server on port " <> show port
    Warp.run port (serverApp handle chanId)
