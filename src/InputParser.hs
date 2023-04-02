module InputParser
  ( Error(..)
  , FormatMatch(..)
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

-- | Defines how input strings are parsed according to the format.
data FormatMatch
  = ExactMatch
  -- ^ requires that the input strings exactly match the format string, this is
  -- faster and works for uniform input
  | LenientMatch
  -- ^ searches for time that matches the format string inside the input strings,
  -- this is much slower and (mostly) works for input with extra data

parseTimes :: FormatMatch -> Text -> [Text] -> Either Error [TimeItem]
parseTimes match format strings = forM strings $ \string ->
  maybe (Left $ InvalidTime string) (Right . TimeItem string)
    . parseFunc
    $ T.unpack string

  where
    parseFunc :: String -> Maybe Time.LocalTime
    parseFunc = case match of
      ExactMatch -> parseTimeM formatString
      LenientMatch -> firstJust . possiblyParsedTimes

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
      -- the time string that a format string parses can't be shorter than the
      -- format string, so we don't need to suggest shorter strings
      let minOutputLength = length formatString

      -- FIXME this isn't a very functional or safe solution
      dropPrefixN <- [0..length string]
      dropSuffixN <- [0..length string]
      -- instead of the absent `dropLast` function, we'll `take` a smaller
      -- number of characters from the beginning, having the same effect
      let outputLength = length string - (dropPrefixN + dropSuffixN)

      guard $ outputLength > minOutputLength
      pure . take outputLength $ drop dropPrefixN string

    formatString = T.unpack format

firstJust :: Foldable t => t (Maybe a) -> Maybe a
firstJust = getFirst . foldMap First

parseTimeM :: MonadFail m => String -> String -> m Time.LocalTime
parseTimeM = Time.parseTimeM acceptWhitespace Time.defaultTimeLocale

acceptWhitespace :: Bool
acceptWhitespace = False
