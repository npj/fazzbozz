module Fazzbozz (
  fazzbozz,
  sfazzbozz,

  FazzState(..),
  scanM,
  makeState,

  ModuloState(..),
  isModulo,

  FibonacciState(..),
  isFibonacci,
  fibs,
  defaultFibonacciState,

  HappyState(..),
  isHappy,
  defaultHappyState,

  EnclosedState(..),
  enclose,
) where

import Fazzbozz.Base
import Fazzbozz.Core
import Fazzbozz.Matches
import Fazzbozz.Simple
