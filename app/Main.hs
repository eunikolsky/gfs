module Main where

import Config (Config(..))
import GFS (gfsRemove, mkTimeList, unTimeList)
import InputParser (Error(..), parseTimes)
import OptParse (configParser)

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time.LocalTime (LocalTime, getZonedTime, zonedTimeToLocalTime)
import Options.Applicative ((<**>), execParser, fullDesc, helper, info, progDesc)
import System.Exit (die)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as TIO (getContents)

main :: IO ()
main = parseOptions >>= printRemoveTimes

parseOptions :: IO Config
parseOptions = execParser $ info (configParser <**> helper)
  ( fullDesc
  <> progDesc "Prints the input times that should be removed for the GFS algorithm"
  )

printRemoveTimes :: Config -> IO ()
printRemoveTimes config = runStderrLoggingT $ do
  inputStrings <- liftIO $ T.lines <$> TIO.getContents
  case mkTimeList <$> parseTimes (cfgTimeFormat config) inputStrings of
    Right inputTimes -> do
      now <- liftIO getLocalTime
      removeTimes <- unTimeList <$> gfsRemove (cfgGFSRanges config) now inputTimes
      liftIO $ forM_ removeTimes print -- FIXME print the input type
    Left (InvalidTime time) -> liftIO . die $ mconcat ["Couldn't parse time from string '", show time, "'"]

getLocalTime :: IO LocalTime
getLocalTime = zonedTimeToLocalTime <$> getZonedTime
