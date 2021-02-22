module Pill5Answers where

import Prelude
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Pill5Types

intParser :: String -> String -> Parser Int
intParser helpText name =
  Parser [ helpText ] \input -> case Map.lookup name input of
    Just [ value ] -> case Int.fromString value of
      Just int -> Right int
      Nothing -> Left $ "Value of option \"" <> name <> "\" is not a valid integer"
    Just [] -> Left $ "Option \"" <> name <> "\" has no value"
    Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
    Nothing -> Left $ "Missing option \"" <> name <> "\""

stringParser :: String -> String -> Parser String
stringParser helpText name =
  Parser [ helpText ] \input -> case Map.lookup name input of
    Just [ value ] -> Right value
    Just [] -> Left $ "Option \"" <> name <> "\" has no value"
    Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
    Nothing -> Left $ "Missing option \"" <> name <> "\""

schemeParser :: String -> String -> Parser Scheme
schemeParser helpText key =
  Parser [ helpText ] \input -> case f input of
    Right "http" -> Right Http
    Right "https" -> Right Https
    Right value -> Left $ "Expected option \"" <> key <> "\" to have value \"http\" or \"https\"."
    Left e -> Left e
  where
  Parser _ f = stringParser helpText key

-- This parser and all of the below are basically specialized versions of
-- `liftN` (in this case `lift3`). So, you can as well define this function
-- like this:
-- ```
-- serverParser = lift3 Server
-- ```
serverParser :: Parser Scheme -> Parser String -> Parser Int -> Parser Server
serverParser schemeParser hostParser portParser =
  Server
    <$> schemeParser
    <*> hostParser
    <*> portParser

credentialsParser :: Parser String -> Parser String -> Parser Credentials
credentialsParser loginParser passwordParser =
  { login: _, password: _ }
    <$> loginParser
    <*> passwordParser

optionsParser :: Parser Server -> Parser Credentials -> Parser Options
optionsParser serverParser credentialsParser =
  Options
    <$> serverParser
    <*> credentialsParser
