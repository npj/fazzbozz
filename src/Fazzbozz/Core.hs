module Fazzbozz.Core (
  sfazzbozz,
  scanM,
  Labeled(..),
) where

import Control.Monad
import Data.Maybe
import Data.Tuple (swap)

import Fazzbozz.Base

type Label = String
data Labeled s = Labeled s Label deriving (Eq, Show)

instance Functor Labeled where
  fmap f (Labeled s l) = Labeled (f s) l

sfazzbozz :: FazzState s => [Labeled s] -> Integer -> (String, [Labeled s])
sfazzbozz ss n = mapFst collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzOne n

fazzOne :: FazzState s => Integer -> Labeled s -> (Maybe String, Labeled s)
fazzOne n = swap . fmap labelToMaybe . dupLabel . fmap (flip matchFazz $ n)

dupLabel :: Labeled (a, b) -> (Labeled a, Labeled b)
dupLabel (Labeled (a, b) lab) = (Labeled a lab, Labeled b lab)

labelToMaybe :: Labeled Bool -> Maybe String
labelToMaybe (Labeled b label) = label <$ guard b

scanM :: Foldable t => (a -> b -> (c, a)) -> a -> t b -> [c]
scanM f s ns = fst $ foldM f' s ns
  where f' s n = mapFst pure $ f s n
