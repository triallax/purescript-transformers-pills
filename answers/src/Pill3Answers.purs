module Pill3Answers where

import Prelude
import Data.Either (Either(..))
import Pill3Types
import Pill2Answers

schemeParser :: String -> Parser Scheme
schemeParser key input = case stringParser key input of
  Right "http" -> Right Http
  Right "https" -> Right Https
  Right value -> Left $ "Expected option \"" <> key <> "\" to have value \"http\" or \"https\"."
  Left e -> Left e

serverParser :: Parser Scheme -> Parser String -> Parser Int -> Parser Server
serverParser schemeParser hostParser portParser input = do
  scheme <- schemeParser input
  host <- hostParser input
  port <- portParser input
  pure $ Server scheme host port

-- Another version
serverParser1 :: Parser Scheme -> Parser String -> Parser Int -> Parser Server
serverParser1 schemeParser hostParser portParser input =
  Server
    <$> schemeParser input
    <*> hostParser input
    <*> portParser input

credentialsParser :: Parser String -> Parser String -> Parser Credentials
credentialsParser loginParser passwordParser input =
  { login: _, password: _ }
    <$> loginParser input
    <*> passwordParser input

optionsParser :: Parser Server -> Parser Credentials -> Parser Options
optionsParser serverParser credentialsParser input =
  Options
    <$> serverParser input
    <*> credentialsParser input
