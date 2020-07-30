{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-wai-logger"
, dependencies =
  [ "console"
  , "effect"
  , "node-process"
  , "numbers"
  , "psci-support"
  , "record-format"
  , "wai"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
