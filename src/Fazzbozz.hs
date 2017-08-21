module Fazzbozz (fazzbozz, simpleMatch) where

simpleMatch :: Int -> String -> Int -> Maybe String
simpleMatch count label n
  | n `mod` count == 0 = Just label
  | otherwise = Nothing

fazzbozz :: [Int -> Maybe String] -> Int -> String
fazzbozz matches val =
  case matchLabels of
    [] -> show val
    otherwise -> foldr1 (++) matchLabels
  where
    matchLabels = labels matches

    labels [] = []
    labels (match : matches) =
      case match val of
        Just label -> label : labels matches
        Nothing -> labels matches
