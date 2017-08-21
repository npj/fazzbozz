module Fazzbozz (fazzbozz, simpleMatch) where

type Match = Int -> Maybe String

simpleMatch :: Int -> String -> Match
simpleMatch count label n
  | n `mod` count == 0 = Just label
  | otherwise = Nothing

fazzbozz :: [Match] -> Int -> String
fazzbozz matches val =
  case maybeMatches of
    Nothing -> show val
    Just matchString -> matchString
  where
    maybeMatches = mconcat matches val
