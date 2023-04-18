module RangeParser
  ( parseRanges
  ) where

import Control.Monad
import Data.Bifunctor
import GFS
import Text.Parsec

-- | Parses `GFSRanges` from the user.
parseRanges :: String -> Either String GFSRanges
parseRanges = first show . runParser parser () ""

type Parser = Parsec String ()

parser :: Parser GFSRanges
parser = do
  h <- read <$> many1 digit
  void $ string "h:"
  d <- read <$> many1 digit
  void $ char 'd'
  let range = GFSRange { rStep = mkTimeIntervalHours h, rLimit = mkTimeIntervalHours (d * 24) }
  pure $ mkGFSRanges range []
