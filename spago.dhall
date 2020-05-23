{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "password-generator"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "test-unit"
  , "concur-core"
  , "concur-react"
  ]
, packages = ./packages.dhall
}
