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

constState :: (a -> b -> c) -> a -> b -> (a, c)
constState f s x = (s, f s x)

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

newtype HappyState = HappyState (Map.Map Integer Bool) deriving (Eq, Show)

isHappy :: Integer -> Bool
isHappy = snd . matchHappy defaultHappyState

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
enclose s = EnclosedState $ \n -> mapFst enclose $ matchFazz s n

matchEnclosed :: EnclosedState -> Integer -> (EnclosedState, Bool)
matchEnclosed (EnclosedState f) n = f n

instance FazzState EnclosedState where
  matchFazz = matchEnclosed
