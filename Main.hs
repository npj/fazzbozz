#!/usr/bin/env runhaskell

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options { number :: Int }

options :: Parser Options
options = Options
  <$> option auto (
        long "number" <>
        short 'n' <>
        help "number of values to output" <>
        showDefault <>
        value 20 )

opts = info (options <**> helper) (
        fullDesc <>
        progDesc "Print fizzbuzz numbers" <>
        header "fazzbozz - an overengineered fizzbuzz" )

main = printFazzbozz =<< execParser opts

printFazzbozz :: Options -> IO ()
printFazzbozz (Options n) = do
  mapM (putStrLn . fazzbozz) [1..n]
  return ()

fazzbozz :: Int -> String
fazzbozz val
  | val `mod` 15 == 0 = "fazzbozz"
  | val `mod` 3 == 0 = "fazz"
  | val `mod` 5 == 0 = "bozz"
  | otherwise = show val
