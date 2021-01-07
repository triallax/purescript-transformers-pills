module Pill2Answers where

import Prelude
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Pill2Types (Parser)

intParser :: String -> Parser Int
-- intParser :: String -> Input -> Either String Int
intParser name input = case Map.lookup name input of
  Just [ value ] -> case Int.fromString value of
    Just int -> Right int
    Nothing -> Left $ "Value of option \"" <> name <> "\" is not a valid integer"
  Just [] -> Left $ "Option \"" <> name <> "\" has no value"
  Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
  Nothing -> Left $ "Missing option \"" <> name <> "\""

stringParser :: String -> Parser String
-- stringParser :: String -> Input -> Either String String
stringParser name input = case Map.lookup name input of
  Just [ value ] -> Right value
  Just [] -> Left $ "Option \"" <> name <> "\" has no value"
  Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
  Nothing -> Left $ "Missing option \"" <> name <> "\""
