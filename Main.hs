#!/usr/bin/env runhaskell

import Data.Semigroup ((<>))
import Options.Applicative

import Fazzbozz
import Options

programOpts = info (options <**> helper) (
        fullDesc <>
        progDesc "Print fizzbuzz numbers" <>
        header "fazzbozz - an overengineered fizzbuzz" )

main = printFazzbozz =<< execParser programOpts

printFazzbozz :: Options -> IO ()
printFazzbozz (Options n) = do
  mapM (putStrLn . fazzbozz) [1..n]
  return ()
