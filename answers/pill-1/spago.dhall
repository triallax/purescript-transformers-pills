{ name = "pill-1"
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
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
