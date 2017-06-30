#!/usr/bin/env runhaskell

main = mapM (putStrLn . fazzbozz) [1..20]

fazzbozz :: Int -> String
fazzbozz val
  | val `mod` 15 == 0 = "fazzbozz"
  | val `mod` 3 == 0 = "fazz"
  | val `mod` 5 == 0 = "bozz"
  | otherwise = show val
