module CmdOptions (
    CmdOptions(..),
    MatchSpecifier,
    MatchPredicateSpecifier(..),
    opts
  ) where

import Control.Monad
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Read (readMaybe)

type PatternParser = [String] -> [MatchSpecifier]
type MatchSpecifier = (String, MatchPredicateSpecifier)

data MatchPredicateSpecifier =
  ModuloPredicate Int |
  FibonacciPredicate
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
    ("fazz", ModuloPredicate 3),
    ("bozz", ModuloPredicate 5)
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

parseSimplePattern :: (String -> Maybe t) -> (t -> MatchPredicateSpecifier) -> PatternParser
parseSimplePattern parseName makeMatchPred args = do
  [rawName, label] <- return args
  Just val <- return $ parseName rawName
  return $ (label, makeMatchPred val)

parseNamedPattern :: String -> MatchPredicateSpecifier -> PatternParser
parseNamedPattern name matchPred = parseSimplePattern parseName $ const matchPred
  where parseName = guard <$> (== name)

-- parsers

patternParsers :: [PatternParser]
patternParsers = [parseCountPattern, parseFibonacciPattern]

parseCountPattern :: PatternParser
parseCountPattern = parseSimplePattern readMaybe ModuloPredicate

parseFibonacciPattern :: PatternParser
parseFibonacciPattern = parseNamedPattern "fib" FibonacciPredicate
