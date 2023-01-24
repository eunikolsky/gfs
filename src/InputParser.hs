module InputParser
  ( Error(..)
  , parseTimes
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Time.Format
import Data.Time.LocalTime
import qualified Data.Text as T

newtype Error = InvalidTime Text
  deriving (Eq, Show)

parseTimes :: Text -> [Text] -> Either Error [LocalTime]
parseTimes format strings = forM strings $ \string ->
  maybe (Left $ InvalidTime string) Right
    . parseTimeM acceptWhitespace defaultTimeLocale formatString $ T.unpack string

  where
    acceptWhitespace = False
    formatString = T.unpack format
