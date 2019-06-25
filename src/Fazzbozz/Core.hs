module Fazzbozz.Core (
  sfazzbozz,
  statefulScan,
  LabeledState(..),
) where

import Control.Monad
import Data.Maybe

import Fazzbozz.Base

type Label = String
data LabeledState s = LabeledState s Label

sfazzbozz :: FazzState s => [LabeledState s] -> Integer -> ([LabeledState s], String)
sfazzbozz ss n = mapSnd collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzWithLabel n

fazzWithLabel :: FazzState s => Integer -> LabeledState s -> (LabeledState s, Maybe String)
fazzWithLabel n (LabeledState s label) = (LabeledState s' label, result)
  where
    (s', result) = mapSnd labelWhen $ matchFazz s n
    labelWhen maybeMatch = label <$ guard maybeMatch

statefulScan :: (a -> b -> (a, c)) -> a -> [b] -> [c]
statefulScan f init [] = []
statefulScan f init (val : vals) = result : statefulScan f newMatcher vals
  where (newMatcher, result) = f init val
