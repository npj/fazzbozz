module CmdOptions (
    CmdOptions(..),
    cmdOptions,
    Pattern(..),
    parsePattern,
    opts
  ) where

import Data.Semigroup ((<>))
import Text.Read

import Options.Applicative

data CmdOptions = CmdOptions {
    number :: Int,
    patterns :: [Pattern]
  } deriving (Show, Eq)

cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> option auto (
        long "number" <>
        short 'n' <>
        help "number of values to output" <>
        showDefault <>
        value 20 )
  <*> (fmap fillDefaultPatterns $ many $ option readPattern (
        long "pattern" <>
        short 'p' <>
        help "show a label at some frequency" <>
        metavar "FREQ:LABEL" ))

data Pattern = Pattern {
    count :: Int,
    label :: String
  } deriving (Show, Eq)

readPattern :: ReadM Pattern
readPattern = eitherReader parsePattern

parsePattern :: String -> Either String Pattern
parsePattern opt =
  case (maybeCount, rawLabel) of
    (Just count, ':' : label) -> Right (Pattern count label)
    otherwise -> Left "Pattern must be FREQ:LABEL"
  where
    (rawCount, rawLabel) = break (':' ==) opt
    maybeCount = readMaybe rawCount

defaultPatterns = [
    Pattern 3 "fazz",
    Pattern 5 "bozz"
  ]

fillDefaultPatterns [] = defaultPatterns
fillDefaultPatterns p = p

opts :: ParserInfo CmdOptions
opts = info (cmdOptions <**> helper) (
        fullDesc <>
        progDesc "Print fizzbuzz numbers" <>
        header "fazzbozz - an overengineered fizzbuzz" )
