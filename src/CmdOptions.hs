module CmdOptions (
    CmdOptions(..),
    makeOpts,
    PatternParser
  ) where

import Data.Semigroup ((<>))

import Data.List.Split (splitOn)
import Options.Applicative
import Text.Read (readMaybe)

import Matching

type PatternParser = [String] -> [Match]

data CmdOptions = CmdOptions {
    number :: Int,
    matches :: [Match]
  }

makeCmdOptions :: [PatternParser] -> [Match] -> Parser CmdOptions
makeCmdOptions patterns defaultMatches = CmdOptions
    <$> option auto (
          long "number" <>
          short 'n' <>
          help "number of values to output" <>
          showDefault <>
          value 20 )
    <*> (fmap fillDefaultMatches $ many $ option readPattern (
          long "pattern" <>
          short 'p' <>
          help "show a label on some pattern" <>
          metavar "PAT:LABEL" ))
  where fillDefaultMatches = fillDefaults defaultMatches
        readPattern = makePatternReader patterns

fillDefaults :: [a] -> [a] -> [a]
fillDefaults defaults [] = defaults
fillDefaults _ p = p

makePatternReader :: [PatternParser] -> ReadM Match
makePatternReader patterns = eitherReader $ parseOption patterns

parseOption :: [PatternParser] -> String -> Either String Match
parseOption patternParsers opt =
  case matches of
    match : _ -> Right match
    [] -> Left "Pattern must be PAT:LABEL"
  where
    patternArguments = splitOn ":" opt
    matches = mconcat $ patternParsers <*> [patternArguments]

makeOpts :: [PatternParser] -> [Match] -> ParserInfo CmdOptions
makeOpts patterns defaultMatches = 
  info (makeCmdOptions patterns defaultMatches <**> helper) (
      fullDesc <>
      progDesc "Print fizzbuzz numbers" <>
      header "fazzbozz - an overengineered fizzbuzz" )
