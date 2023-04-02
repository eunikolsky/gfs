{-# LANGUAGE ApplicativeDo #-}

module OptParse
  ( configParser
  ) where

import Config (Config(..), defaultRanges)
import InputParser (FormatMatch(..))

import Options.Applicative (Parser, flag, help, long, short, strOption, switch)

configParser :: Parser Config
configParser = do
  timeFormat <- strOption
    ( long "format"
    <> short 'f'
    <> help "Time format to parse input strings"
    )

  verbose <- switch
    ( long "verbose"
    <> short 'v'
    <> help "Print verbose logs"
    )

  formatMatch <- flag ExactMatch LenientMatch
    ( long "lenient-match"
    <> help "Search for time inside input strings (slower)"
    )

  pure $ Config
    { cfgTimeFormat = timeFormat
    , cfgGFSRanges = defaultRanges
    , cfgVerbose = verbose
    , cfgFormatMatch = formatMatch
    }
