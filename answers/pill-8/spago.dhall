{ name = "pill-6"
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
  , "validation"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
