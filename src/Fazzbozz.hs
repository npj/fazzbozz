module Fazzbozz (
  fazzbozz,
  sfazzbozz,
  statefulScan,
  voidState,

  isModulo,
  isFibonacci,
  fibs,
  bindFibonacci,

  StatelessMatcher,
  StatefulMatcher,
  BoundMatcher(..)
) where

import Control.Monad
import Data.Maybe

type StatelessMatcher a = a -> Bool
type StatefulMatcher ps a = ps -> a -> (ps, Bool)

data BoundMatcher ps a = BoundMatcher {
    matchName :: String,
    matchUpdate :: StatefulMatcher ps a,
    matchState :: ps
  }

feedMatcher :: BoundMatcher ps a -> a -> (BoundMatcher ps a, Bool)
feedMatcher m val = (m { matchState = newState }, isMatch)
  where (newState, isMatch) = (matchUpdate m) (matchState m) val

sfazzbozz :: Show a => [BoundMatcher ps a] -> a -> ([BoundMatcher ps a], String)
sfazzbozz matchers val = (newMatchers, resultStr)
  where
    (newMatchers, matchResults) = unzip $ map runMatch matchers
    runMatch bmatcher = fmap addLabel $ feedMatcher bmatcher val
      where addLabel isMatch = matchName bmatcher <$ guard isMatch
    resultStr = fromMaybe (show val) $ mconcat matchResults

statefulScan :: (st -> a -> (st, b)) -> st -> [a] -> [b]
statefulScan f istate [] = []
statefulScan f istate (val : vals) = result : statefulScan f newstate vals
  where (newstate, result) = f istate val

fazzbozz :: Show a => [(String, StatelessMatcher a)] -> a -> String
fazzbozz preds = snd . sfazzbozz spreds
  where
    spreds = map bindState preds
    bindState (s, m) = BoundMatcher s (voidState m) ()

voidState :: StatelessMatcher a -> StatefulMatcher st a
voidState pred s val = (s, pred val)

-- matches

isModulo :: Integral a => a -> a -> Bool
isModulo count = (== 0) <$> (`mod` count)

dropElem :: Ord a => [a] -> a -> ([a], Bool)
dropElem ns n
  | n' == n = (rest, True)
  | otherwise = (ns', False)
  where ns'@(n':rest) = dropWhile (<n) ns

fibs :: Num n => [n]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFibonacci :: (Ord a, Num a) => a -> Bool
isFibonacci n = snd $ dropElem fibs n

bindFibonacci :: (Num n, Ord n) => String -> BoundMatcher [n] n
bindFibonacci label = BoundMatcher label dropElem fibs
