module Main where

import Config (Config(..))
import GFS (TimeItem(itStr), gfsRemove, mkTimeList, unTimeList)
import InputParser (Error(..), parseTimes)
import OptParse (configParser)

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel(..), filterLogger, logErrorN, runStderrLoggingT)
import Data.Time.LocalTime (LocalTime(..), getZonedTime, timeOfDayToTime, timeToTimeOfDay, zonedTimeToLocalTime)
import Options.Applicative ((<**>), execParser, fullDesc, helper, info, progDesc)
import System.Exit (exitFailure)
import qualified Data.Text as T (lines, pack)
import qualified Data.Text.IO as TIO (getContents, putStrLn)

main :: IO ()
main = parseOptions >>= printRemoveTimes

parseOptions :: IO Config
parseOptions = execParser $ info (configParser <**> helper)
  ( fullDesc
  <> progDesc "Prints the input times that should be removed for the GFS algorithm"
  )

printRemoveTimes :: Config -> IO ()
printRemoveTimes config = runLogging . (>>= logError) . runExceptT $ do
  inputStrings <- liftIO $ T.lines <$> TIO.getContents
  inputTimes <- fmap mkTimeList . ExceptT . pure $ parseTimes (cfgTimeFormat config) inputStrings
  now <- liftIO getLocalTime
  removeTimes <- unTimeList <$> gfsRemove (cfgGFSRanges config) now inputTimes
  let removeItems = itStr <$> removeTimes
  liftIO $ forM_ removeItems TIO.putStrLn

  where
    logError (Left (InvalidTime time)) = do
      logErrorN $ mconcat ["Couldn't parse time from string '", T.pack $ show time, "'"]
      -- it's interesting that the error log in the function is still printed even though
      -- `exitFailure` terminates the program here
      liftIO exitFailure
    logError (Right ()) = pure ()

    runLogging = runStderrLoggingT . filterLoggerForVerbose
    filterLoggerForVerbose = if cfgVerbose config then passAllLogs else passInfoAndHigher
    passAllLogs = id -- everything is logged by default
    passInfoAndHigher = filterLogger $ const (>= LevelInfo)

getLocalTime :: IO LocalTime
getLocalTime = roundToSeconds . zonedTimeToLocalTime <$> getZonedTime
  where
    roundToSeconds = modifyTimeOfDay $ realToFrac @Int . floor
    modifyTimeOfDay f t@(LocalTime _ tod) = t
      { localTimeOfDay = timeToTimeOfDay . f . timeOfDayToTime $ tod }
