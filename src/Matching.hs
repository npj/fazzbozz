module Matching (Match, simpleMatch) where

import Control.Monad
import Data.Maybe

type Match = Int -> Maybe String

simpleMatch :: Int -> String -> Match
simpleMatch count label n = label <$ guard (n `mod` count == 0)
