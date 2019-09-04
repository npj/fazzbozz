module Fazzbozz.Core (
  sfazzbozz,
  scanM,
) where

import Control.Monad
import Data.Maybe
import Data.Tuple (swap)

import Fazzbozz.Base

type Label = String

sfazzbozz :: FazzState s => [(Label, s)] -> Integer -> (String, [(Label, s)])
sfazzbozz ss n = mapFst collectResults $ unzip $ fazzAll ss
  where
    collectResults = fromMaybe (show n) . mconcat
    fazzAll = map $ fazzOne n

fazzOne :: FazzState s => Integer -> (Label, s) -> (Maybe String, (Label, s))
fazzOne n = swap . fmap labelToMaybe . dupFst . fmap (flip matchFazz $ n)

dupFst :: (a, (b, c)) -> ((a, b), (a, c))
dupFst (a, (b, c)) = ((a, b), (a, c))

labelToMaybe :: (Label, Bool) -> Maybe String
labelToMaybe (label, b) = label <$ guard b

scanM :: Foldable t => (a -> b -> (c, a)) -> a -> t b -> [c]
scanM f s ns = fst $ foldM f' s ns
  where f' s n = mapFst pure $ f s n
