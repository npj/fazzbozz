module Fazzbozz.Core (
  sfazzbozz,
  scanM,
  LabeledState(..),
) where

import Control.Monad
import Data.Maybe

import Fazzbozz.Base

type Label = String
data LabeledState s = LabeledState s Label deriving (Eq, Show)

sfazzbozz :: FazzState s => [LabeledState s] -> Integer -> (String, [LabeledState s])
sfazzbozz ss n = mapFst collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzWithLabel n

fazzWithLabel :: FazzState s => Integer -> LabeledState s -> (Maybe String, LabeledState s)
fazzWithLabel n (LabeledState s label) = (result, LabeledState s' label)
  where
    (s', result) = mapSnd labelWhen $ matchFazz s n
    labelWhen maybeMatch = label <$ guard maybeMatch

scanM :: Foldable t => (a -> b -> (c, a)) -> a -> t b -> [c]
scanM f s ns = fst $ foldM f' s ns
  where f' s n = mapFst pure $ f s n
