module Bot where

import qualified Colog

import           Control.Concurrent             (forkIO)

import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T

import           Discord
import           Discord.Internal.Types.Channel (Message (..))
import           Discord.Internal.Types.Events  (Event (..))
import qualified Discord.Requests               as Discord (ChannelRequest (CreateMessage))

import           Types
import           Web.Habitica
import           WebServer                      (runServer)

runBot :: Text -> Env -> IO ()
runBot discordToken env@Env {..} =
    void $ Discord.runDiscord botOpts
  where
    logDebug = Colog.usingLoggerT envLogger . Colog.logDebug
    logInfo = Colog.usingLoggerT envLogger . Colog.logInfo
    botOpts = def
        { discordToken = discordToken
        , discordOnStart =
            void . forkIO . runServer envPort envSystemMessagesChannelId envGeneralChannelId envLogger
        , discordOnEnd = logInfo "Bot shutting down."
        , discordOnEvent = \handle -> \case
            MessageCreate msg -> handleMessageCreate msg handle env
            ev -> logDebug $ show ev
        , discordOnLog = logInfo
        }

handleMessageCreate :: Message -> DiscordHandle -> Env -> IO ()
handleMessageCreate Message {..} handle Env {..} = do
    when ("!lastCrons" `T.isPrefixOf` messageText) $ do
        logInfo "Processing !lastCrons request"
        eitherMembers <- responseBody <$>
            runHabiticaRequest envHabiticaAuth defaultHabiticaHttpConfig (getGroupMembers GroupParty)

        case eitherMembers of
            Left err -> do
                logError $ show err
            Right members -> do
                let
                    crons =
                        fmap (\Member {..} ->
                            palUsername (pauthLocal memberAuth)
                            <> ": "
                            <> show (patsLoggedIn $ pauthTimestamps memberAuth)
                        ) members
                    msg = "Last crons were...\n" <> mconcat (intersperse "\n" crons)
                result <- Discord.restCall handle $ Discord.CreateMessage messageChannel msg
                case result of
                    Right _  -> logInfo "Success!"
                    Left err -> logError $ "Err: " <> show err

    when ("!questProgress" `T.isPrefixOf` messageText) $ do
        logInfo "Processing !questProgress request"
        eitherMembers <- responseBody <$>
            runHabiticaRequest envHabiticaAuth defaultHabiticaHttpConfig (getGroupMembers GroupParty)
        case eitherMembers of
            Left err -> do
                logError $ show err
            Right members -> do
                let
                    questProgs =
                        fmap (\m ->
                            let
                                username = palUsername $ pauthLocal $ memberAuth m
                                questProg = questProgress $ partyQuest $ memberParty m
                                bossQuestMsg =
                                    username
                                    <> " has "
                                    <> show (qpUp questProg)
                                    <> " pending damage"
                            in
                            case qpCollect questProg of
                                Nothing -> -- boss quest
                                    bossQuestMsg
                                Just collect ->
                                    if Map.null collect
                                    then bossQuestMsg -- boss quest
                                    else let -- collection quest
                                        collected =
                                            Map.foldlWithKey' (\acc item n ->
                                                acc <> " " <> item <> "(" <> show n <> ")"
                                            ) "" collect
                                    in
                                    username
                                    <> " has collected: "
                                    <> collected
                        ) members
                    msg = mconcat $ intersperse "\n" $ "Quest progress:" : questProgs
                result <- Discord.restCall handle $ Discord.CreateMessage messageChannel msg
                case result of
                    Right _  -> logInfo "Success!"
                    Left err -> logError $ "Err: " <> show err
  where
    logInfo = Colog.usingLoggerT envLogger . Colog.logInfo
    logError = Colog.usingLoggerT envLogger . Colog.logError
