module Test.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Main as M
import Test.Assert (assertEqual)

-- Replace these with your own code.
intParser = M.intParser

stringParser = M.stringParser

tupleParser = M.tupleParser

fold = M.fold

input = Map.fromFoldable [ "scheme" /\ [ "https" ], "host" /\ [ "example.com" ], "port" /\ [ "8000" ], "login" /\ [ "admin" ], "password" /\ [ "hunter2" ] ]

assertEqual' actual expected = assertEqual { actual, expected }

main :: Effect Unit
main = do
  log "intParser"
  assertEqual' (intParser "port" input) (Right 8000)
  assertEqual' (intParser "port" $ Map.fromFoldable [ "pord" /\ [ "8000" ] ]) (Left "Missing option \"port\"")
  assertEqual' (intParser "host" input) (Left "Value of option \"host\" is not a valid integer")
  assertEqual' (intParser "port" $ Map.fromFoldable [ "port" /\ [ "8000", "8080" ] ]) (Left "Expected option \"port\" to have one value only")
  assertEqual' (intParser "port" $ Map.fromFoldable [ "port" /\ [] ]) (Left "Option \"port\" has no value")

  log "stringParser"
  assertEqual' (stringParser "host" input) (Right "example.com")
  assertEqual' (stringParser "login" input) (Right "admin")
  assertEqual' (stringParser "password" input) (Right "hunter2")
  assertEqual' (stringParser "whatever" input) (Left "Missing option \"whatever\"")
  assertEqual' (stringParser "host" $ Map.fromFoldable [ "port" /\ [] ]) (Left "Missing option \"host\"")
  assertEqual' (stringParser "host" $ Map.fromFoldable [ "host" /\ [ "example.com", "purescript.org" ] ]) (Left "Expected option \"host\" to have one value only")

  log "tupleParser"
  assertEqual' (tupleParser (stringParser "login") (stringParser "password") input) (Right ("admin" /\ "hunter2"))
  assertEqual' (tupleParser (stringParser "host") (intParser "port") input) (Right ("example.com" /\ 8000))
  assertEqual' (tupleParser (intParser "host") (stringParser "port") input) (Left "Value of option \"host\" is not a valid integer")

  log "fold"
  assertEqual'
    ( fold
        [ "--x"
        , "1"
        , "--y"
        , "string"
        , "another string"
        , "--z"
        ]
    )
    $ Just
    $ Map.fromFoldable [ "x" /\ [ "1" ], "y" /\ [ "string", "another string" ], "z" /\ [] ]
  assertEqual' (fold [ "x" ]) Nothing
  assertEqual' (fold []) $ Just Map.empty
