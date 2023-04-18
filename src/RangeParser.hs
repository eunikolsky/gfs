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
  rStep <- timeIntervalParser
  void $ char ':'
  rLimit <- timeIntervalParser

  let range = GFSRange { rStep, rLimit }
  pure $ mkGFSRanges range []

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
