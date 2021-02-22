module Pill4Types (Scheme(..), Server(..), Credentials, Options(..), Parser, Input) where

import Prelude
import Data.Map (Map)
import Data.Tuple (Tuple)
import Data.Either (Either)

type Input
  = Map String (Array String)

type Parser a
  = Tuple (Array String) (Input -> Either String a)

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
