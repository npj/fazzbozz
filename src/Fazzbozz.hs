module Fazzbozz (
  fazzbozz,
  MatchPredicate,
  LabeledPredicate,
  isModulo,
  isFibonacci
) where

import Control.Monad
import Data.Maybe

type LabeledPredicate = (String, MatchPredicate)
type MatchPredicate = Int -> Bool

fazzbozz :: [LabeledPredicate] -> Int -> String
fazzbozz preds = fromMaybe <$> show <*> mconcat boundMatches
  where
    boundMatches = map bindPredicate preds
    bindPredicate (label, p) n = label <$ guard (p n)

isModulo :: Int -> MatchPredicate
isModulo count = (== 0) <$> (`mod` count)

memberOfOrderedList :: (Eq a, Ord a) => [a] -> a -> Bool
memberOfOrderedList ls n = n `elem` takeWhile (<= n) ls

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFibonacci :: MatchPredicate
isFibonacci = memberOfOrderedList fibs
