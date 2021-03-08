module Pill6Answers where

import Pill6Types
import Prelude
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V(..), invalid)

intParser :: String -> String -> Parser Int
intParser helpText name =
  Parser [ helpText ] \input -> case Map.lookup name input of
    Just [ value ] -> case Int.fromString value of
      Just int -> pure int
      Nothing -> invalid [ "Value of option \"" <> name <> "\" is not a valid integer" ]
    Just [] -> invalid [ "Option \"" <> name <> "\" has no value" ]
    Just _ -> invalid [ "Expected option \"" <> name <> "\" to have one value only" ]
    Nothing -> invalid [ "Missing option \"" <> name <> "\"" ]

stringParser :: String -> String -> Parser String
stringParser helpText name =
  Parser [ helpText ] \input -> case Map.lookup name input of
    Just [ value ] -> pure value
    Just [] -> invalid [ "Option \"" <> name <> "\" has no value" ]
    Just _ -> invalid [ "Expected option \"" <> name <> "\" to have one value only" ]
    Nothing -> invalid [ "Missing option \"" <> name <> "\"" ]

schemeParser :: String -> String -> Parser Scheme
schemeParser helpText key =
  Parser [ helpText ] \input -> case f input of
    V (Right "http") -> pure Http
    V (Right "https") -> pure Https
    V (Right value) -> invalid [ "Expected option \"" <> key <> "\" to have value \"http\" or \"https\"." ]
    V (Left e) -> invalid e
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
