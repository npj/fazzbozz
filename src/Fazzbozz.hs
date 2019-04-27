module Fazzbozz (
  fazzbozz,
  sfazzbozz,

  FazzState(..),
  statefulScan,

  ModuloState(..),
  isModulo,

  FibonacciState(..),
  isFibonacci,
  fibs,
  defaultFibonacciState,

  HappyState(..),
  isHappy,
  defaultHappyState,

  EnclosedState(..),
  enclose,
) where

import Control.Monad
import Data.Maybe
import Data.Tuple

import qualified Data.Map as Map

-- core fazzbozz logic, in state-passing and simplified stateless forms

fazzbozz :: [(String, Integer -> Bool)] -> Integer -> String
fazzbozz preds = snd . sfazzbozz states
  where
    states = map makeState preds
    makeState (label, pred) = (PredicateState pred, label)

sfazzbozz :: FazzState s => [(s, String)] -> Integer -> ([(s, String)], String)
sfazzbozz ss n = mapSnd collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzWithLabel n

fazzWithLabel :: FazzState s => Integer -> (s, String) -> ((s, String), Maybe String)
fazzWithLabel n (s, label) = ((s', label), result)
  where
    (s', result) = mapSnd labelWhen $ matchFazz s n
    labelWhen maybeMatch = label <$ guard maybeMatch

-- types and functions for statefully evaluating numbers for fazzbozz
-- pattern matching. I bet once I really grok monads this section will
-- mostly melt away.

class FazzState s where
  matchFazz :: s -> Integer -> (s, Bool)

statefulScan :: (a -> b -> (a, c)) -> a -> [b] -> [c]
statefulScan f init [] = []
statefulScan f init (val : vals) = result : statefulScan f newMatcher vals
  where (newMatcher, result) = f init val

-- helpers

constState :: (a -> b -> c) -> a -> b -> (a, c)
constState f s x = (s, f s x)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

expandUntil :: (a -> (a, b)) -> (a -> Bool) -> a -> [b]
expandUntil f done val
  | done val = []
  | otherwise =
    let (val', result) = f val
        rest = expandUntil f done val'
    in result : rest

--
-- matches
--

-- predicate

newtype PredicateState = PredicateState (Integer -> Bool)

instance FazzState PredicateState where
  matchFazz = constState matchPredicate
    where matchPredicate (PredicateState f) = f

-- modulo

newtype ModuloState = ModuloState Integer deriving (Show)

isModulo :: Integral a => a -> a -> Bool
isModulo count n = (n `mod` count) == 0

instance FazzState ModuloState where
  matchFazz = constState matchModulo
    where matchModulo (ModuloState n) = isModulo n

-- fibonacci

newtype FibonacciState = FibonacciState [Integer] deriving (Show)

isFibonacci :: (Ord a, Num a) => a -> Bool
isFibonacci = snd . dropElem fibs

dropElem :: Ord a => [a] -> a -> ([a], Bool)
dropElem ns n
  | n' == n = (rest, True)
  | otherwise = (ns', False)
  where ns'@(n':rest) = dropWhile (<n) ns

fibs :: Num n => [n]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

defaultFibonacciState :: FibonacciState
defaultFibonacciState = FibonacciState fibs

matchFibonacci :: FibonacciState -> Integer -> (FibonacciState, Bool)
matchFibonacci (FibonacciState s) n = mapFst FibonacciState $ dropElem s n

instance FazzState FibonacciState where
  matchFazz = matchFibonacci

-- happy

newtype HappyState = HappyState (Map.Map Integer Bool) deriving (Show)

isHappy :: Integer -> Bool
isHappy = snd . matchHappy defaultHappyState

nextHappy :: Integral a => a -> a
nextHappy = sum . map square . digits 10
  where square n = n * n

digits :: Integral a => a -> a -> [a]
digits base = expandUntil (`divMod` base) (== 0)

defaultHappyState :: HappyState
defaultHappyState = HappyState $ Map.fromList [(1, True), (4, False)]

matchHappy :: HappyState -> Integer -> (HappyState, Bool)
matchHappy (HappyState s) = mapFst HappyState . collectLookup nextHappy s

collectLookup :: Ord a => (a -> a) -> Map.Map a b -> a -> (Map.Map a b, b)
collectLookup f m val = collectLookup' present m val
  where
    present = Map.lookup val m
    collectLookup' (Just result) m _ = (m, result)
    collectLookup' Nothing m val = (m', result)
      where
        (m', result) = mapFst insertVal $ collectLookup f m $ f val
        insertVal = Map.insert val result

instance FazzState HappyState where
  matchFazz = matchHappy

-- enclosed

newtype EnclosedState = EnclosedState (Integer -> (EnclosedState, Bool))

enclose :: FazzState s => s -> EnclosedState
enclose s = EnclosedState f
  where
    f n = (es', result)
      where
        (s', result) = matchFazz s n
        es' = enclose s'

matchEnclosed :: EnclosedState -> Integer -> (EnclosedState, Bool)
matchEnclosed (EnclosedState f) n = f n

instance FazzState EnclosedState where
  matchFazz = matchEnclosed
