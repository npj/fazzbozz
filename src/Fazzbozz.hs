module Fazzbozz (
  fazzbozz,
  Match,
  matchModulo,
  matchFibonacci
) where

import Control.Monad
import Data.Maybe

type Match = Int -> Maybe String

fazzbozz :: [Match] -> Int -> String
fazzbozz matches = fromMaybe <$> show <*> mconcat matches

liftMatch :: (Int -> Bool) -> String -> Match
liftMatch p label n = label <$ guard (p n)

matchModulo :: Int -> String -> Match
matchModulo count = liftMatch $ (== 0) <$> (`mod` count)

memberOfOrderedList :: (Eq a, Ord a) => [a] -> a -> Bool
memberOfOrderedList ls n = n `elem` takeWhile (<= n) ls

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

matchFibonacci :: String -> Match
matchFibonacci = liftMatch $ memberOfOrderedList fibs
