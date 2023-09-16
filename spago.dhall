{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "concur-core"
  , "concur-react"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "react"
  , "strings"
  , "strings-extra"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
