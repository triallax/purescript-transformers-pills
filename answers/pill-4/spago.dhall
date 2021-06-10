{ name = "pill-4"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "either"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
