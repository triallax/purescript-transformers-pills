module Pill5Types where

import Data.Either
import Data.Tuple
import Prelude
import Control.Alternative (class Apply)
import Data.Map (Map)

type Input
  = Map String (Array String)

data Parser a
  = Parser (Array String) (Input -> Either String a)

derive instance functorParser :: Functor Parser

-- TODO: We should probably provide a hint for this (e.g. partial implementation or
-- simpler `Parser` type).
instance applyParser :: Apply Parser where
  apply (Parser help1 input2EitherA2b) (Parser help2 input2EitherB) =
    Parser
      (help1 <> help2) \input -> input2EitherA2b input <*> input2EitherB input

instance applicativeParser :: Applicative Parser where
  pure x = Parser [] (const (Right x))

-- instance bindParser :: Bind Parser where
-- You can try for yourself, but it's impossible.
-- bind ... = ...

data Scheme
  = Http
  | Https

derive instance eqScheme :: Eq Scheme

-- Server $SCHEME $HOST $PORT
data Server
  = Server Scheme String Int

derive instance eqServer :: Eq Server

type Credentials
  = { login :: String, password :: String }

data Options
  = Options Server Credentials

derive instance eqOptions :: Eq Options
