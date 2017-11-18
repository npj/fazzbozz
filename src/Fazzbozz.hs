module Fazzbozz (
  mfazzbozz,
  fazzbozz,
  Matchable(..),
  isModulo,
  isFibonacci
) where

import Control.Monad
import Data.Maybe

class Matchable m where
  match :: m -> Int -> Bool

mfazzbozz :: (Matchable m) => [(String, m)] -> Int -> String
mfazzbozz preds = fromMaybe <$> show <*> mconcat boundMatches
  where
    boundMatches = map bindPredicate preds
    bindPredicate (label, p) n = label <$ guard (match p $ n)

fazzbozz :: [(String, Int -> Bool)] -> Int -> String
fazzbozz preds = mfazzbozz $ mapSnd MatchPredicate preds
  where
    mapSnd f = map (\(a, b) -> (a, f b))

newtype MatchPredicate = MatchPredicate { getMatch :: Int -> Bool }

instance Matchable MatchPredicate where
  match = getMatch

-- matches

isModulo :: Int -> Int -> Bool
isModulo count = (== 0) <$> (`mod` count)

memberOfOrderedList :: (Eq a, Ord a) => [a] -> a -> Bool
memberOfOrderedList ls n = n `elem` takeWhile (<= n) ls

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFibonacci :: Int -> Bool
isFibonacci = memberOfOrderedList fibs
