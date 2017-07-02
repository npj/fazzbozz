module CmdOptions (CmdOptions(..), parseOptions, optParseInfo) where

import Options.Applicative
import Data.Semigroup ((<>))

data CmdOptions = CmdOptions { number :: Int } deriving (Show, Eq)

parseOptions :: Parser CmdOptions
parseOptions = CmdOptions
  <$> option auto (
        long "number" <>
        short 'n' <>
        help "number of values to output" <>
        showDefault <>
        value 20 )

optParseInfo :: ParserInfo CmdOptions
optParseInfo = info (parseOptions <**> helper) (
        fullDesc <>
        progDesc "Print fizzbuzz numbers" <>
        header "fazzbozz - an overengineered fizzbuzz" )
