module Fazzbozz (fazzbozz, simpleMatch) where

type Match = Int -> Maybe String

simpleMatch :: Int -> String -> Match
simpleMatch count label n
  | n `mod` count == 0 = Just label
  | otherwise = Nothing

fazzbozz :: [Match] -> Int -> String
fazzbozz matches val =
  case matchLabels of
    [] -> show val
    otherwise -> concat matchLabels
  where
    matchLabels = labels matches

    labels [] = []
    labels (match : matches) =
      case match val of
        Just label -> label : labels matches
        Nothing -> labels matches
