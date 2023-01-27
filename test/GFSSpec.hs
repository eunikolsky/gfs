module GFSSpec where

import GFS

import Control.Monad
import Control.Monad.Logger
import Data.Functor.Identity
import Data.List ((\\))
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec =
  describe "gfsRemove" $ do
    let now = read "2023-11-19 22:00:00"
        hour = mkTimeInterval 0 1
        day = mkTimeInterval 0 24
        month = mkTimeInterval 1 0
        year = mkTimeInterval 12 0
        hourly = GFSRange hour day
        daily = GFSRange day month
        monthly = GFSRange month year
        -- see the expected checkpoints in a test in `GFS.Internal.GFSRangeSpec`
        ranges = mkGFSRanges hourly [daily, monthly]

        times = mkTimeList $ fmap ((TimeItem "") . read)
          -- too old
          [ "2020-01-01 03:00:23", "2022-11-19 00:00:00"

          -- monthly:
          -- in the oldest month, including the oldest possible time to keep
          , "2022-11-19 22:00:00", "2022-11-20 00:00:00", "2022-12-16 14:53:21"
          -- one month before the oldest
          , "2022-12-19 22:00:00", "2022-12-31 23:45:00"
          -- the newest of the months
          , "2023-09-18 20:33:01"

          -- daily:
          , "2023-10-19 23:00:00", "2023-10-20 12:00:00"
          , "2023-10-20 22:00:00", "2023-10-20 22:04:00", "2023-10-21 05:04:00", "2023-10-21 21:04:00"
          , "2023-11-08 16:00:00"
          , "2023-11-17 16:00:00", "2023-11-17 21:23:00"

          -- hourly:
          , "2023-11-18 22:00:00", "2023-11-18 22:55:00"
          , "2023-11-18 23:00:01", "2023-11-18 23:13:00"
          , "2023-11-19 03:07:29"
          , "2023-11-19 09:08:00", "2023-11-19 09:18:00", "2023-11-19 09:58:08"
          -- the newest hour before now
          , "2023-11-19 21:43:00", "2023-11-19 21:47:00"
          -- just before now
          , "2023-11-19 21:59:59"

          -- newer than now
          , "2023-11-19 23:45:45", "2023-12-01 01:45:45"
          , "2023-12-19 02:00:00", "2023-12-19 02:15:00"
          , "2023-12-31 23:59:59"
          ]

    context "removes correct times (examples)" $
      it "basic example" $ do
        -- created from `times` by keeping all in "too old", removing the first
        -- time on every line after that, removing the newest time before now,
        -- and removing all "newer than now"
        let expectedRemoved = mkTimeList $ fmap ((TimeItem "") . read)
              -- too old
              [ "2020-01-01 03:00:23", "2022-11-19 00:00:00"

              -- monthly:
              , "2022-11-20 00:00:00", "2022-12-16 14:53:21"
              -- one month before the oldest
              , "2022-12-31 23:45:00"

              -- daily:
              , "2023-10-20 12:00:00"
              , "2023-10-20 22:04:00", "2023-10-21 05:04:00", "2023-10-21 21:04:00"
              , "2023-11-17 21:23:00"

              -- hourly:
              , "2023-11-18 22:55:00"
              , "2023-11-18 23:13:00"
              , "2023-11-19 09:18:00", "2023-11-19 09:58:08"
              -- the newest hour before now
              , "2023-11-19 21:47:00"
              ]

        runNoLoggingT (gfsRemove ranges now times) `shouldBe` Identity expectedRemoved

    it "shifting now by month keeps \"most\" of old data (example)" $ do
      -- I think it's more obvious for a human to see what's left, not what's removed,
      -- after two cleanups in this test
      let shiftedNowByMonth = addTimeInterval month now -- "2023-12-19 22:00:00"
          expectedLeft = mkTimeList $ fmap ((TimeItem "") . read)
            -- monthly [2022-12-19 22:00:00; 2023-11-19 22:00:00):
            [ "2022-12-19 22:00:00"
            , "2023-09-18 20:33:01"
            , "2023-10-19 23:00:00"

            -- daily [2023-11-19 22:00:00; 2023-12-18 22:00:00):
            , "2023-11-19 23:45:45"
            , "2023-12-01 01:45:45"

            -- hourly [2023-12-18 22:00:00; 2023-12-19 22:00:00):
            , "2023-12-19 02:00:00"
            , "2023-12-19 02:15:00" -- the newest before now

            -- newer than now [2023-12-19 22:00:00; ∞):
            , "2023-12-31 23:59:59"
            ]

      leftAfterNow <- runNoLoggingT $ gfsLeft ranges now times
      leftAfterMonth <- runNoLoggingT $ gfsLeft ranges shiftedNowByMonth leftAfterNow
      leftAfterMonth `shouldBe` expectedLeft

    it "shifting now day-by-day for a month is the same as shifting now by month (example)" $ do
      let shiftedNowByMonth = addTimeInterval month now

      leftAfterMonth <- runNoLoggingT $ gfsLeft ranges shiftedNowByMonth times

      let numDays = diffDays (localDay shiftedNowByMonth) (localDay now)
          -- note: the range starts with `0`, not `1`, so that the kept times match those from
          -- `leftAfterMonth`; otherwise, `2022-12-19 22:00:00` is missing from `leftAfterDays`;
          -- this change seems correct in this case, but may not be correct for all cases
          shiftedNowsByDays = (flip addLocalTime now) . (* nominalDay) . realToFrac <$> [0..numDays]
      leftAfterDays <- runNoLoggingT $ foldM (\newTimes newNow -> gfsLeft ranges newNow newTimes) times shiftedNowsByDays

      leftAfterDays `shouldBe` leftAfterMonth

gfsLeft :: MonadLogger m => GFSRanges -> Now -> TimeList -> m TimeList
gfsLeft ranges now times = do
  removed <- gfsRemove ranges now times
  pure . mkTimeList $ unTimeList times \\ unTimeList removed
