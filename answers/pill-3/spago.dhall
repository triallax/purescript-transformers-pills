{ name = "pill-3"
, dependencies =
  [ "prelude"
  , "arrays"
  , "either"
  , "assert"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "psci-support"
  , "strings"
  , "tuples"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
