module RangeParser
  ( parseRanges
  ) where

import GFS

-- | Parses `GFSRanges` from the user.
parseRanges :: String -> Either String GFSRanges
parseRanges "" = Left ""
parseRanges _ = Right $ mkGFSRanges (GFSRange (mkTimeIntervalHours 1) (mkTimeIntervalHours 24)) []
