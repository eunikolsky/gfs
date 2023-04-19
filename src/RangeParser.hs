module RangeParser
  ( parseRanges
  ) where

import Control.Monad
import Data.Bifunctor
import Data.List (uncons)
import Data.Maybe
import GFS
import Text.Parsec hiding (uncons)

-- | Parses `GFSRanges` from the user.
parseRanges :: String -> Either String GFSRanges
parseRanges = first show . runParser parser () ""

type Parser = Parsec String ()

parser :: Parser GFSRanges
parser = do
  ranges <- sepBy1 rangeParser (char ',')
  let (range, rest) = fromMaybe err $ uncons ranges
      err = error "Impossible: empty list from `sepBy1`"
  pure $ mkGFSRanges range rest

rangeParser :: Parser GFSRange
rangeParser = do
  rStep <- timeIntervalParser
  void $ char ':'
  rLimit <- timeIntervalParser

  pure $ GFSRange { rStep, rLimit }

timeIntervalParser :: Parser TimeInterval
timeIntervalParser = do
  num <- read <$> many1 digit
  mkTimeInterval <- choice
    [ mkTimeIntervalHours <$ char 'h'
    , mkTimeIntervalDays <$ char 'd'
    , mkTimeIntervalWeeks <$ char 'w'
    , mkTimeIntervalMonths <$ char 'm'
    , mkTimeIntervalYears <$ char 'y'
    ]
  pure $ mkTimeInterval num
