module Pill1Types where

import Data.Map (Map)
import Data.Maybe (Maybe)

type Input
  = Map String (Array String)

type Parser a
  = Input -> Maybe a
