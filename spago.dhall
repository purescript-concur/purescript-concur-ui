{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "concur-core"
  , "concur-react"
  , "console"
  , "effect"
  , "integers"
  , "psci-support"
  , "strings-extra"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
