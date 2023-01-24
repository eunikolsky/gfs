module InputParser
  ( parseTimes
  ) where

import Data.Text (Text)
import Data.Time.Format
import Data.Time.LocalTime
import qualified Data.Text as T

parseTimes :: Text -> [Text] -> [LocalTime]
parseTimes format strings =
  parseTimeOrError acceptWhitespace defaultTimeLocale formatString . T.unpack <$> strings

  where
    acceptWhitespace = False
    formatString = T.unpack format
