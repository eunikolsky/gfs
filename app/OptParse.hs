{-# LANGUAGE ApplicativeDo #-}

module OptParse
  ( actionParser
  ) where

import Config (Action(..), Config(..), defaultRanges)
import InputParser (FormatMatch(..))

import Options.Applicative ((<|>), Parser, flag, flag', help, long, short, strOption, switch)

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
