module InputParser
  ( Error(..)
  , parseTimes
  ) where

import GFS (TimeItem(..))

import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Data.Time.Format qualified as Time
import Data.Time.LocalTime qualified as Time
import qualified Data.Text as T

newtype Error = InvalidTime Text
  deriving (Eq, Show)

parseTimes :: Text -> [Text] -> Either Error [TimeItem]
parseTimes format strings = forM strings $ \string ->
  maybe (Left $ InvalidTime string) (Right . TimeItem string)
    . firstJust
    . possiblyParsedTimes
    $ T.unpack string

  where
    -- | Produces a (lazy) list of possibly parsed times by progressively
    -- dropping longer and longer prefix from the input string: w/o 0 first
    -- chars, w/o 1 first char, w/o 2 first chars, etc.
    --
    -- Note: dropping the prefix from a `Text` might be faster, but each such
    -- text has to be unpacked to `String` for `parseTimeM`, so unpacking it
    -- once may be faster overall.
    possiblyParsedTimes :: String -> [Maybe Time.LocalTime]
    possiblyParsedTimes string = parseTimeM formatString . flip drop string
      -- TODO no need to go to an empty string
      -- FIXME this isn't a very functional or safe solution
      <$> [0..length string]

    formatString = T.unpack format

firstJust :: Foldable t => t (Maybe a) -> Maybe a
firstJust = getFirst . foldMap First

parseTimeM :: MonadFail m => String -> String -> m Time.LocalTime
parseTimeM = Time.parseTimeM acceptWhitespace Time.defaultTimeLocale

acceptWhitespace :: Bool
acceptWhitespace = False
