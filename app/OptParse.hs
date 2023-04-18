{-# LANGUAGE ApplicativeDo #-}

module OptParse
  ( actionParser
  ) where

import Config (Action(..), Config(..), defaultRanges)
import InputParser (FormatMatch(..))
import RangeParser (parseRanges)

import Options.Applicative ((<|>), Parser, eitherReader, flag, flag', help, long, option, short, showDefault, strOption, switch, value)

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

  pure $ Config
    { cfgTimeFormat = timeFormat
    , cfgGFSRanges = ranges
    , cfgVerbose = verbose
    , cfgFormatMatch = formatMatch
    }
