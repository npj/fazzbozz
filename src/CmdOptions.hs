module CmdOptions (
    CmdOptions(..),
    MatchSpecifier(..),
    opts,
    parseCountPattern,
    parseFibonacciPattern
  ) where

import Control.Monad
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Read (readMaybe)

type PatternParser = [String] -> [MatchSpecifier]

data MatchSpecifier =
  ModuloMatch Int String |
  FibonacciMatch String
  deriving (Show, Eq)

data CmdOptions = CmdOptions {
    number :: Int,
    matchSpecs :: [MatchSpecifier]
  }

cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
    <$> option auto (
          long "number" <>
          short 'n' <>
          help "number of values to output" <>
          showDefault <>
          value 20 )
    <*> (fmap fillDefaultMatchSpecs $ many $ option readOption (
          long "pattern" <>
          short 'p' <>
          help "show a label on some pattern" <>
          metavar "PAT:LABEL" ))
  where readOption = eitherReader $ parseOption

-- This is almost certainly done for us in a library somewhere
fillDefaultMatchSpecs :: [MatchSpecifier] -> [MatchSpecifier]
fillDefaultMatchSpecs [] = defaultMatchSpecs
fillDefaultMatchSpecs p = p

defaultMatchSpecs :: [MatchSpecifier]
defaultMatchSpecs = [
    ModuloMatch 3 "fazz",
    ModuloMatch 5 "bozz"
  ]

parseOption :: String -> Either String MatchSpecifier
parseOption opt =
  case matches of
    match : _ -> Right match
    [] -> Left "Pattern must be PAT:LABEL"
  where
    patternArguments = splitOn ":" opt
    matches = mconcat $ patternParsers <*> [patternArguments]

opts :: ParserInfo CmdOptions
opts = info (cmdOptions <**> helper) (
          fullDesc <>
          progDesc "Print fizzbuzz numbers" <>
          header "fazzbozz - an overengineered fizzbuzz" )

-- parser utils

parseSimplePattern :: (String -> Maybe t) -> (t -> String -> MatchSpecifier) -> PatternParser
parseSimplePattern parseName makeMatchSpec args = do
  [rawName, label] <- return args
  Just val <- return $ parseName rawName
  return $ makeMatchSpec val label

parseNamedPattern :: String -> (String -> MatchSpecifier) -> PatternParser
parseNamedPattern name makeMatchSpec = parseSimplePattern parseName _makeMatchSpec
  where
    parseName = guard <$> (== name)
    _makeMatchSpec = const makeMatchSpec

-- parsers

patternParsers :: [PatternParser]
patternParsers = [parseCountPattern, parseFibonacciPattern]

parseCountPattern :: PatternParser
parseCountPattern = parseSimplePattern readMaybe ModuloMatch

parseFibonacciPattern :: PatternParser
parseFibonacciPattern = parseNamedPattern "fib" FibonacciMatch
