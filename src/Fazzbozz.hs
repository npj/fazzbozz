module Fazzbozz (
  fazzbozz,
  patternParsers,
  parseCountPattern
) where

import Data.Maybe
import Text.Read

import Matching

fazzbozz :: [Match] -> Int -> String
fazzbozz matches = fromMaybe <$> show <*> mconcat matches

-- parsers

patternParsers :: [[String] -> [Match]]
patternParsers = [parseCountPattern]

parseCountPattern :: [String] -> [Match]
parseCountPattern [rawCount, label] =
  case readMaybe rawCount of
    Just count -> [simpleMatch count label]
    Nothing -> []
parseCountPattern _ = []
