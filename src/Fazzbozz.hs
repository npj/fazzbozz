module Fazzbozz (
  fazzbozz,
  Match(..),
  isModulo,
  isFibonacci
) where

import Control.Monad
import Data.Maybe

type Match a = a -> Bool

fazzbozz :: Show a => [(String, Match a)] -> a -> String
fazzbozz preds = fromMaybe <$> show <*> mconcat boundMatches
  where
    boundMatches = map bindPredicate preds
    bindPredicate (label, m) n = label <$ guard (m n)

-- matches

isModulo :: Integral a => a -> a -> Bool
isModulo count = (== 0) <$> (`mod` count)

memberOfOrderedList :: (Eq a, Ord a) => [a] -> a -> Bool
memberOfOrderedList ls n = n `elem` takeWhile (<= n) ls

fibs :: (Num n) => [n]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFibonacci :: (Num n, Ord n) => n -> Bool
isFibonacci = memberOfOrderedList fibs
