{ name = "pill-2"
, dependencies =
  [ "prelude"
  , "assert"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "psci-support"
  , "strings"
  , "tuples"
  , "arrays"
  , "either"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
