module Fazzbozz (
  fazzbozz,
  patternParsers,
  parseCountPattern,
  parseFibonacciPattern
) where

import Control.Monad
import Data.Maybe

import Text.Read

import Matching

fazzbozz :: [Match] -> Int -> String
fazzbozz matches = fromMaybe <$> show <*> mconcat matches

-- parser utils

type PatternParser = [String] -> [Match]

parseSimplePattern :: (String -> Maybe t) -> (t -> String -> Match) -> PatternParser
parseSimplePattern parseName makeMatch args = do
  [rawName, label] <- return args
  Just val <- return $ parseName rawName
  return $ makeMatch val label

parseNamedPattern :: String -> (String -> Match) -> PatternParser
parseNamedPattern name match = parseSimplePattern (guard <$> (== name)) (const match)

-- parsers

patternParsers :: [PatternParser]
patternParsers = [parseCountPattern, parseFibonacciPattern]

parseCountPattern :: PatternParser
parseCountPattern = parseSimplePattern readMaybe simpleMatch

parseFibonacciPattern = parseNamedPattern "fib" matchFibonacci
