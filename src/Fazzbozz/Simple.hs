module Fazzbozz.Simple (
  fazzbozz,
) where

import Fazzbozz.Core
import Fazzbozz.Matches

fazzbozz :: [(String, Integer -> Bool)] -> Integer -> String
fazzbozz preds = snd . sfazzbozz states
  where
    states = map makeState preds
    makeState (label, pred) = LabeledState (PredicateState pred) label
