module CmdOptions (CmdOptions(..), cmdOptions, opts) where

import Options.Applicative
import Data.Semigroup ((<>))

data CmdOptions = CmdOptions { number :: Int } deriving (Show, Eq)

cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> option auto (
        long "number" <>
        short 'n' <>
        help "number of values to output" <>
        showDefault <>
        value 20 )

opts :: ParserInfo CmdOptions
opts = info (cmdOptions <**> helper) (
        fullDesc <>
        progDesc "Print fizzbuzz numbers" <>
        header "fazzbozz - an overengineered fizzbuzz" )
