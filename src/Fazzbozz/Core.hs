module Fazzbozz.Core (
  sfazzbozz,
  scanM,
  Labeled(..),
) where

import Control.Monad
import Data.Maybe

import Fazzbozz.Base

type Label = String
data Labeled s = Labeled s Label deriving (Eq, Show)

sfazzbozz :: FazzState s => [Labeled s] -> Integer -> (String, [Labeled s])
sfazzbozz ss n = mapFst collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzWithLabel n

fazzWithLabel :: FazzState s => Integer -> Labeled s -> (Maybe String, Labeled s)
fazzWithLabel n (Labeled s label) = (result, Labeled s' label)
  where
    (s', result) = mapSnd labelWhen $ matchFazz s n
    labelWhen maybeMatch = label <$ guard maybeMatch

scanM :: Foldable t => (a -> b -> (c, a)) -> a -> t b -> [c]
scanM f s ns = fst $ foldM f' s ns
  where f' s n = mapFst pure $ f s n
