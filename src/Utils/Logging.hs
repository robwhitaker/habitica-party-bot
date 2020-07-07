{-# LANGUAGE RecordWildCards #-}

module Utils.Logging where

import qualified System.Directory as Dir
import qualified System.FilePath  as FilePath
import qualified System.IO        as IO

import           Chronos          (Time)
import qualified Chronos
import           Colog            (LogAction, Message, Msg (..), RichMsg (..))
import qualified Colog

import qualified Data.Text        as T

squares :: Text -> Text
squares t =
    " [" <> t <> "] "

fmtTime :: Time -> Text
fmtTime =
    squares
    . Chronos.encode_YmdHMS (Chronos.SubsecondPrecisionFixed 0) Chronos.w3c
    . Chronos.timeToDatetime

fmtSourceLoc :: Text -> Text
fmtSourceLoc txt =
    case T.split (=='#') txt of
        [modulePath, lineNum] ->
            mconcat
                [ T.intercalate "."
                    . reverse
                    . drop 1
                    . reverse
                    $ T.split (=='.') modulePath
                , "@"
                , lineNum
                ]
        _ -> txt

fmtRichMessage :: MonadIO m => RichMsg m Message -> m Text
fmtRichMessage msg =
    Colog.fmtRichMessageCustomDefault msg $
        \_ mbTime Colog.Msg {..} ->
            Colog.showSeverity msgSeverity
            <> maybe "" fmtTime mbTime
            <> fmtSourceLoc (Colog.showSourceLoc msgStack)
            <> msgText

terminalLogger :: MonadIO m => LogAction m Message
terminalLogger = Colog.upgradeMessageAction Colog.defaultFieldMap
    $ Colog.cfilter (\(RichMsg (Msg sev _ _) _) -> sev > Colog.Debug)
    $ Colog.cmapM (fmap encodeUtf8 . fmtRichMessage) Colog.logByteStringStdout

fileLogger :: MonadIO m => Handle -> LogAction m Message
fileLogger logFileH = Colog.upgradeMessageAction Colog.defaultFieldMap
    $ Colog.cmapM (fmap encodeUtf8 . fmtRichMessage) (Colog.logByteStringHandle logFileH)

runWithBackgroundLogger :: MonadIO m => FilePath -> (LogAction m Message -> IO a) -> IO a
runWithBackgroundLogger logFile f = do
    Dir.createDirectoryIfMissing True (FilePath.takeDirectory logFile)
    withFile logFile AppendMode $ \logFileH -> do
        IO.hSetBuffering logFileH IO.LineBuffering
        IO.hSetBuffering stdout IO.LineBuffering
        Colog.withBackgroundLogger
            Colog.defCapacity
            (terminalLogger <> fileLogger logFileH)
            f
