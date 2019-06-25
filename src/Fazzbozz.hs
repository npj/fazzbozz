module Fazzbozz (
  fazzbozz,
  sfazzbozz,

  LabeledState(..),
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

import Fazzbozz.Base
import Fazzbozz.Core
import Fazzbozz.Matches

fazzbozz :: [(String, Integer -> Bool)] -> Integer -> String
fazzbozz preds = snd . sfazzbozz states
  where
    states = map makeState preds
    makeState (label, pred) = LabeledState (PredicateState pred) label
