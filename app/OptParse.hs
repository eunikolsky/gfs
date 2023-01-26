{-# LANGUAGE ApplicativeDo #-}

module OptParse
  ( configParser
  ) where

import Config (Config(..), defaultRanges)

import Options.Applicative (Parser, help, long, short, strOption, switch)

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
  pure $ Config
    { cfgTimeFormat = timeFormat
    , cfgGFSRanges = defaultRanges
    , cfgVerbose = verbose
    }
