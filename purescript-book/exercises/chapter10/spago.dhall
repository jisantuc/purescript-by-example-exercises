{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "pairs"
  , "psci-support"
  , "react-basic-hooks"
  , "react-basic-dom"
  , "test-unit"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
