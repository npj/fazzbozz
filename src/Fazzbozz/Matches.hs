module Fazzbozz.Matches (
  PredicateState(..),

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

import Data.List
import Data.Tuple

import qualified Data.Map as Map

import Fazzbozz.Base

-- helpers

constState :: (a -> b -> c) -> a -> b -> (c, a)
constState f s x = (f s x, s)

-- predicate

newtype PredicateState = PredicateState (Integer -> Bool)

instance FazzState PredicateState where
  matchFazz = constState matchPredicate
    where matchPredicate (PredicateState f) = f

-- modulo

newtype ModuloState = ModuloState Integer deriving (Eq, Show)

isModulo :: Integral a => a -> a -> Bool
isModulo count n = (n `mod` count) == 0

instance FazzState ModuloState where
  matchFazz = constState matchModulo
    where matchModulo (ModuloState n) = isModulo n

-- fibonacci


newtype FibonacciState = FibonacciState [Integer] deriving (Eq, Show)

isFibonacci :: (Ord a, Num a) => a -> Bool
isFibonacci = fst . dropElem fibs

dropElem :: Ord a => [a] -> a -> (Bool, [a])
dropElem ns n
  | n' == n = (True, rest)
  | otherwise = (False, ns')
  where ns'@(n':rest) = dropWhile (<n) ns

fibs :: Num n => [n]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

defaultFibonacciState :: FibonacciState
defaultFibonacciState = FibonacciState fibs

matchFibonacci :: FibonacciState -> Integer -> (Bool, FibonacciState)
matchFibonacci (FibonacciState s) n = fmap FibonacciState $ dropElem s n

instance FazzState FibonacciState where
  matchFazz = matchFibonacci

-- happy

newtype HappyState = HappyState (Map.Map Integer Bool) deriving (Eq, Show)

isHappy :: Integer -> Bool
isHappy = fst . matchHappy defaultHappyState

nextHappy :: Integral a => a -> a
nextHappy = sum . map square . digits 10
  where square n = n * n

digits :: Integral a => a -> a -> [a]
digits base = unfoldr $ digits' base
  where
    digits' _ 0 = Nothing
    digits' base n = Just . swap $ n `divMod` base

defaultHappyState :: HappyState
defaultHappyState = HappyState $ Map.fromList [(1, True), (4, False)]

matchHappy :: HappyState -> Integer -> (Bool, HappyState)
matchHappy (HappyState s) = fmap HappyState . collectLookup nextHappy s

collectLookup :: Ord a => (a -> a) -> Map.Map a b -> a -> (b, Map.Map a b)
collectLookup f m val = collectLookup' present m val
  where
    present = Map.lookup val m
    collectLookup' (Just result) m _ = (result, m)
    collectLookup' Nothing m val = (result, m')
      where
        (result, m') = fmap insertVal $ collectLookup f m $ f val
        insertVal = Map.insert val result

instance FazzState HappyState where
  matchFazz = matchHappy

-- enclosed

newtype EnclosedState = EnclosedState (Integer -> (Bool, EnclosedState))

enclose :: FazzState s => s -> EnclosedState
enclose s = EnclosedState $ \n -> fmap enclose $ matchFazz s n

matchEnclosed :: EnclosedState -> Integer -> (Bool, EnclosedState)
matchEnclosed (EnclosedState f) n = f n

instance FazzState EnclosedState where
  matchFazz = matchEnclosed
