module Pill3Types (module Exports, Scheme(..), Server(..), Credentials, Options(..)) where

import Prelude
import Pill2Types
import Pill2Types as Exports

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
