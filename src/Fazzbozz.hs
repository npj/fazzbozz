module Fazzbozz (fazzbozz) where

fazzbozz :: Int -> String
fazzbozz val
  | val `mod` 15 == 0 = "fazzbozz"
  | val `mod` 3 == 0 = "fazz"
  | val `mod` 5 == 0 = "bozz"
  | otherwise = show val
