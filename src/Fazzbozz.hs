module Fazzbozz (fazzbozz, simpleMatch) where

import Control.Monad
import Data.Maybe

type Match = Int -> Maybe String

simpleMatch :: Int -> String -> Match
simpleMatch count label n = label <$ guard (n `mod` count == 0)

fazzbozz :: [Match] -> Int -> String
fazzbozz matches = fromMaybe <$> show <*> mconcat matches
