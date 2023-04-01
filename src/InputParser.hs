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
    -- dropping longer and longer prefix and/or suffix from the input string.
    --
    -- Note: dropping the prefix/suffix from a `Text` might be faster, but each
    -- such text has to be unpacked to `String` for `parseTimeM`, so unpacking it
    -- once may be faster overall.
    possiblyParsedTimes :: String -> [Maybe Time.LocalTime]
    possiblyParsedTimes = fmap (parseTimeM formatString) . candidates

    -- FIXME optimize this? property-based tests are noticeably slower
    candidates string = do
      -- TODO no need to go to an empty string
      -- FIXME this isn't a very functional or safe solution
      dropPrefixN <- [0..length string]
      dropSuffixN <- [0..length string]
      -- instead of the absent `dropLast` function, we'll `take` a smaller
      -- number of characters from the beginning, having the same effect
      let takeN = length string - (dropPrefixN + dropSuffixN)
      pure . take takeN $ drop dropPrefixN string

    formatString = T.unpack format

firstJust :: Foldable t => t (Maybe a) -> Maybe a
firstJust = getFirst . foldMap First

parseTimeM :: MonadFail m => String -> String -> m Time.LocalTime
parseTimeM = Time.parseTimeM acceptWhitespace Time.defaultTimeLocale

acceptWhitespace :: Bool
acceptWhitespace = False
