module GFS
    ( cleanup
    ) where

import Data.Time.LocalTime

-- |The main cleanup function that takes a sorted list of times and returns
-- |the ones that should be removed to satisfy the requirements of the GFS
-- |algorithm.
-- |__Assumption__: the input list of times is sorted in the ascending order
-- |(oldest to newest).
cleanup :: [LocalTime] -> [LocalTime]
cleanup = const []
