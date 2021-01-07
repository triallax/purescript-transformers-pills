module Pill2Types where

import Data.Either (Either)
import Data.Map (Map)

type Input
  = Map String (Array String)

type Parser a
  = Input -> Either String a
