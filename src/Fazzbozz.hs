module Fazzbozz (
  fazzbozz,
  sfazzbozz,

  SimpleMatcher,
  StatefulMatcher,
  ChainingMatcher(..),
  BoundMatcher(..),

  makeStateful,
  makeChaining,
  chainSimpleMatcher,
  bindLabel,
  bindSimpleMatcher,
  matchTogether,
  statefulScan,

  isModulo,
  moduloMatcher,

  isFibonacci,
  fibs,
  fibonacciMatcher,
) where

import Control.Monad
import Data.Maybe

-- core fazzbozz logic, in state-passing and simplified stateless forms

fazzbozz :: Show a => [(String, SimpleMatcher a)] -> a -> String
fazzbozz preds = snd . sfazzbozz matcher
  where
    matcher = matchTogether $ map (uncurry bindSimpleMatcher) preds
    makeMatcher (label, matcher) = bindSimpleMatcher label matcher

sfazzbozz :: Show a => BoundMatcher a -> a -> (BoundMatcher a, String)
sfazzbozz matcher val = (nextMatcher, fazzResult)
  where
    (nextMatcher, boundResult) = nextBoundMatch matcher val
    fazzResult = fromMaybe (show val) boundResult

-- types and functions for statefully evaluating numbers for fazzbozz
-- pattern matching. I bet once I really grok monads this section will
-- mostly melt away.

type SimpleMatcher a = a -> Bool
type StatefulMatcher st a = st -> a -> (st, Bool)

newtype ChainingMatcher a = ChainingMatcher {
  nextMatch :: a -> (ChainingMatcher a, Bool)
}

newtype BoundMatcher a = BoundMatcher {
  nextBoundMatch :: a -> (BoundMatcher a, Maybe String)
}

makeStateful :: SimpleMatcher a -> StatefulMatcher () a
makeStateful m () a = ((), m a)

makeChaining :: StatefulMatcher st a -> st -> ChainingMatcher a
makeChaining smatcher initState = ChainingMatcher $ chain initState
  where
    chain state val = (nextChain, result)
      where
        (nextState, result) = smatcher state val
        nextChain = ChainingMatcher $ chain nextState

chainSimpleMatcher :: SimpleMatcher a -> ChainingMatcher a
chainSimpleMatcher matcher = (makeChaining . makeStateful) matcher ()

bindLabel :: String -> ChainingMatcher a  -> BoundMatcher a
bindLabel label cmatcher = BoundMatcher $ bnext cmatcher
  where
    bnext chain val = (nextBound, boundResult)
      where
        (nextChain, chainResult) = nextMatch chain val
        nextBound = BoundMatcher $ bnext nextChain
        boundResult = label <$ guard chainResult

bindSimpleMatcher :: String -> SimpleMatcher a -> BoundMatcher a
bindSimpleMatcher label matcher = bindLabel label $ (makeChaining . makeStateful) matcher ()

matchTogether :: [BoundMatcher a] -> BoundMatcher a
matchTogether matchers = BoundMatcher $ bnext matchers
  where
    bnext matchers val = (nextMatcher, concatResult)
      where
        matchResults = map (\m -> nextBoundMatch m val) matchers
        (nextMatchers, stringResults) = unzip matchResults
        nextMatcher = BoundMatcher $ bnext nextMatchers
        concatResult = mconcat stringResults

statefulScan :: (a -> b -> (a, c)) -> a -> [b] -> [c]
statefulScan f init [] = []
statefulScan f init (val : vals) = result : statefulScan f newMatcher vals
  where (newMatcher, result) = f init val

-- matches

isModulo :: Integral a => a -> a -> Bool
isModulo count = (== 0) <$> (`mod` count)

moduloMatcher :: Integral a => a -> ChainingMatcher a
moduloMatcher count = chainSimpleMatcher $ isModulo count

dropElem :: Ord a => [a] -> a -> ([a], Bool)
dropElem ns n
  | n' == n = (rest, True)
  | otherwise = (ns', False)
  where ns'@(n':rest) = dropWhile (<n) ns

fibs :: Num n => [n]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFibonacci :: (Ord a, Num a) => a -> Bool
isFibonacci n = snd $ dropElem fibs n

fibonacciMatcher :: (Ord a, Num a) => ChainingMatcher a
fibonacciMatcher = ChainingMatcher $ matchFibonacci fibs
  where
    matchFibonacci state val = (nextMatcher, result)
      where
        nextMatcher = ChainingMatcher $ matchFibonacci rest
        (rest, result) = dropElem state val

