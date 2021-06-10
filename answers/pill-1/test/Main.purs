module Test.Main (main) where

import Prelude
import Test.Assert (assertEqual)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Main as M

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
  assertEqual' (intParser "port" input) (Just 8000)
  assertEqual' (intParser "pord" input) Nothing
  assertEqual' (intParser "host" input) Nothing
  assertEqual' (intParser "port" $ Map.fromFoldable [ "port" /\ [ "8000", "8080" ] ]) Nothing

  log "stringParser"
  assertEqual' (stringParser "host" input) (Just "example.com")
  assertEqual' (stringParser "login" input) (Just "admin")
  assertEqual' (stringParser "password" input) (Just "hunter2")
  assertEqual' (stringParser "whatever" input) Nothing
  assertEqual' (stringParser "host" $ Map.fromFoldable [ "host" /\ [ "example.com", "purescript.org" ] ]) Nothing

  log "tupleParser"
  assertEqual' (tupleParser (stringParser "login") (stringParser "password") input) (Just ("admin" /\ "hunter2"))
  assertEqual' (tupleParser (stringParser "host") (intParser "port") input) (Just ("example.com" /\ 8000))
  assertEqual' (tupleParser (intParser "host") (stringParser "port") input) Nothing

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
