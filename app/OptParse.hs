{-# LANGUAGE ApplicativeDo #-}

module OptParse
  ( actionParser
  ) where

import Config (Action(..), Config(..), defaultRanges)
import InputParser (FormatMatch(..), parseNow)
import RangeParser (parseRanges)

import Options.Applicative ((<|>), Parser, eitherReader, flag, flag', help, long, maybeReader, option, optional, short, showDefault, strOption, switch, value)

actionParser :: Parser Action
actionParser
  = flag' ShowVersion (long "version" <> help "Show program version")
  <|> RunGFS <$> configParser

configParser :: Parser Config
configParser = do
  timeFormat <- strOption
    ( long "format"
    <> short 'f'
    <> help "Time format to parse input strings"
    )

  ranges <- option (eitherReader parseRanges)
    ( long "ranges"
    <> short 'r'
    <> help "GFS ranges"
    <> value defaultRanges
    <> showDefault
    )

  verbose <- switch
    ( long "verbose"
    <> short 'v'
    <> help "Print verbose logs"
    )

  formatMatch <- flag ExactMatch LenientMatch
    ( long "lenient-match"
    <> help "Search for time inside input strings (might be slower)"
    )

  now <- optional $ option (maybeReader parseNow)
    ( long "now"
    <> help "Override \"now\", in the ISO-8601 format (yyyy-MM-ddTHH:mm:ss)"
    )

  pure $ Config
    { cfgTimeFormat = timeFormat
    , cfgGFSRanges = ranges
    , cfgVerbose = verbose
    , cfgFormatMatch = formatMatch
    , cfgNow = now
    }
