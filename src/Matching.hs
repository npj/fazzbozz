module Matching (
    Match,
    simpleMatch,
    matchFibonacci
  ) where

import Control.Monad
import Data.Maybe

type Match = Int -> Maybe String

liftMatch :: (Int -> Bool) -> String -> Match
liftMatch p label n = label <$ guard (p n)

simpleMatch :: Int -> String -> Match
simpleMatch count = liftMatch $ (== 0) <$> (`mod` count)

memberOfOrderedList :: (Eq a, Ord a) => [a] -> a -> Bool
memberOfOrderedList ls n = n `elem` takeWhile (<= n) ls

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

matchFibonacci :: String -> Match
matchFibonacci = liftMatch $ memberOfOrderedList fibs
