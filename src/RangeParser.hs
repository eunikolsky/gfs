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
  numStep <- read <$> many1 digit
  stepFunc <- choice
    [ mkTimeIntervalHours <$ char 'h'
    , mkTimeIntervalHours . (* 7) . (* 24) <$ char 'w'
    ]
  void $ char ':'
  numLimit <- read <$> many1 digit
  limitFunc <- choice
    [ mkTimeIntervalHours . (* 24) <$ char 'd'
    , mkTimeIntervalMonths <$ char 'm'
    ]
  let range = GFSRange { rStep = stepFunc numStep, rLimit = limitFunc numLimit }
  pure $ mkGFSRanges range []
