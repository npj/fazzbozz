module Fazzbozz.Simple (
  fazzbozz,
  makeState,
) where

import Fazzbozz.CmdOptions
import Fazzbozz.Core
import Fazzbozz.Matches

fazzbozz :: [(String, Integer -> Bool)] -> Integer -> String
fazzbozz preds = snd . sfazzbozz states
  where
    states = map makeState preds
    makeState (label, pred) = LabeledState (PredicateState pred) label

makeState :: MatchPredicateSpecifier Integer -> EnclosedState
makeState (ModuloPredicate n) = enclose $ ModuloState n
makeState FibonacciPredicate = enclose defaultFibonacciState
makeState HappyPredicate = enclose defaultHappyState
