module Options (Options(..), options) where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options { number :: Int }

options :: Parser Options
options = Options
  <$> option auto (
        long "number" <>
        short 'n' <>
        help "number of values to output" <>
        showDefault <>
        value 20 )
