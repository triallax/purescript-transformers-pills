module Test.Main (main) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid)
import Effect (Effect)
import Effect.Console (log)
import Main (Scheme(..), Server(..), Options(..))
import Main as M
import Test.Assert (assertEqual)

-- Replace these with your own code.
intParser = M.intParser

stringParser = M.stringParser

tupleParser = M.tupleParser

schemeParser = M.schemeParser

serverParser = M.serverParser

optionsParser = M.optionsParser

credentialsParser = M.credentialsParser

fold = M.fold

input = Map.fromFoldable [ "scheme" /\ [ "https" ], "host" /\ [ "example.com" ], "port" /\ [ "8000" ], "login" /\ [ "admin" ], "password" /\ [ "hunter2" ] ]

assertEqual' actual expected = assertEqual { actual, expected }

assertParserOutputEqual ::
  forall a.
  Eq a =>
  Show a =>
  M.Parser a ->
  M.Input -> Array String -> V (Array String) a -> Effect Unit
assertParserOutputEqual (M.Parser actualHelpText parserFn) input expectedHelpText expectedOutput = do
  assertEqual' actualHelpText expectedHelpText
  assertEqual' (parserFn input) expectedOutput

main :: Effect Unit
main = do
  log "intParser"
  assertParserOutputEqual
    (intParser [ "help" ] "port")
    input
    [ "help" ]
    (pure 8000)
  assertParserOutputEqual
    (intParser [ "help" ] "port")
    (Map.fromFoldable [ "pord" /\ [ "8000" ] ])
    [ "help" ]
    (invalid [ "Missing option \"port\"" ])
  assertParserOutputEqual
    (intParser [ "help" ] "host")
    input
    [ "help" ]
    (invalid [ "Value of option \"host\" is not a valid integer" ])
  assertParserOutputEqual
    (intParser [ "help" ] "port")
    (Map.fromFoldable [ "port" /\ [ "8000", "8080" ] ])
    [ "help" ]
    (invalid [ "Expected option \"port\" to have one value only" ])
  assertParserOutputEqual
    (intParser [ "help" ] "port")
    (Map.fromFoldable [ "port" /\ [] ])
    [ "help" ]
    (invalid [ "Option \"port\" has no value" ])
  log "stringParser"
  assertParserOutputEqual
    (stringParser [ "help" ] "host")
    input
    [ "help" ]
    (pure "example.com")
  assertParserOutputEqual
    (stringParser [ "help" ] "login")
    input
    [ "help" ]
    (pure "admin")
  assertParserOutputEqual
    (stringParser [ "help" ] "password")
    input
    [ "help" ]
    (pure "hunter2")
  assertParserOutputEqual
    (stringParser [ "help" ] "whatever")
    input
    [ "help" ]
    (invalid [ "Missing option \"whatever\"" ])
  assertParserOutputEqual
    (stringParser [ "help" ] "host")
    (Map.fromFoldable [ "port" /\ [] ])
    [ "help" ]
    (invalid [ "Missing option \"host\"" ])
  assertParserOutputEqual
    (stringParser [ "help" ] "host")
    (Map.fromFoldable [ "host" /\ [ "example.com", "purescript.org" ] ])
    [ "help" ]
    (invalid [ "Expected option \"host\" to have one value only" ])
  log "tupleParser"
  assertParserOutputEqual
    (tupleParser (stringParser [ "a" ] "login") (stringParser [ "b" ] "password"))
    input
    [ "a", "b" ]
    (pure ("admin" /\ "hunter2"))
  assertParserOutputEqual
    (tupleParser (stringParser [ "a" ] "host") (intParser [ "b" ] "port"))
    input
    [ "a", "b" ]
    (pure ("example.com" /\ 8000))
  assertParserOutputEqual
    (tupleParser (intParser [ "a" ] "host") (stringParser [ "b" ] "port"))
    input
    [ "a", "b" ]
    (invalid [ "Value of option \"host\" is not a valid integer" ])
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
  log "schemeParser"
  assertParserOutputEqual
    (schemeParser [ "help" ] "scheme")
    (Map.singleton "scheme" [ "http" ])
    [ "help" ]
    (pure Http)
  assertParserOutputEqual
    (schemeParser [ "help" ] "scheme")
    ((Map.singleton "scheme" [ "https" ]))
    [ "help" ]
    (pure Https)
  assertParserOutputEqual
    (schemeParser [ "help" ] "scheme")
    ((Map.singleton "scheme" [ "hddp" ]))
    [ "help" ]
    (invalid [ "Expected option \"scheme\" to have value \"http\" or \"https\"." ])
  log "serverParser"
  let
    appliedServerParser =
      serverParser
        (schemeParser [ "scheme" ] "scheme")
        (stringParser [ "host" ] "host")
        (intParser [ "port" ] "port")
  assertParserOutputEqual
    appliedServerParser
    (Map.fromFoldable [ "scheme" /\ [ "http" ], "host" /\ [ "example.com" ], "port" /\ [ "8080" ] ])
    [ "scheme", "host", "port" ]
    (pure (Server Http "example.com" 8080))
  log "credentialsParser"
  let
    appliedCredentialsParser =
      credentialsParser
        (stringParser [ "login" ] "login")
        (stringParser [ "password" ] "password")
  assertParserOutputEqual
    appliedCredentialsParser
    (Map.fromFoldable [ "login" /\ [ "admin" ], "password" /\ [ "123456" ] ])
    [ "login", "password" ]
    (pure { login: "admin", password: "123456" })
  log "optionsParser"
  let
    appliedOptionsParser = optionsParser appliedServerParser appliedCredentialsParser
  assertParserOutputEqual
    appliedOptionsParser
    input
    [ "scheme", "host", "port", "login", "password" ]
    (pure $ Options (Server Https "example.com" 8000) { login: "admin", password: "hunter2" })
