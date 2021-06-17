{ name = "pill-7"
, dependencies =
  [ "prelude"
  , "assert"
  , "console"
  , "effect"
  , "maybe"
  , "psci-support"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
