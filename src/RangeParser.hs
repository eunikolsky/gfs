module RangeParser
  ( parseRanges
  ) where

import GFS

-- | Parses `GFSRanges` from the user.
parseRanges :: String -> Either String GFSRanges
parseRanges = const $ Left ""
