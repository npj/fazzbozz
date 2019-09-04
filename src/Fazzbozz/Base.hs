module Fazzbozz.Base (
  FazzState(..),
  mapFst,
  mapSnd,
) where

class FazzState s where
  matchFazz :: s -> Integer -> (Bool, s)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)
