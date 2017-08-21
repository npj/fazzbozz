module Fazzbozz (fazzbozz, simpleMatch) where

import Data.Maybe

type Match = Int -> Maybe String

simpleMatch :: Int -> String -> Match
simpleMatch count label n
  | n `mod` count == 0 = Just label
  | otherwise = Nothing

fazzbozz :: [Match] -> Int -> String
fazzbozz matches = fromMaybe <$> show <*> mconcat matches
