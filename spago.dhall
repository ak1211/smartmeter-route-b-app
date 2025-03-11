{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "smartmetere-route-b-app"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "const"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-bootstrap5"
  , "halogen-subscriptions"
  , "integers"
  , "lists"
  , "maybe"
  , "media-types"
  , "monad-loops"
  , "newtype"
  , "now"
  , "numbers"
  , "parsing"
  , "parsing-dataview"
  , "partial"
  , "precise-datetime"
  , "prelude"
  , "refs"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uint"
  , "unfoldable"
  , "unicode"
  , "uri"
  , "web-clipboard"
  , "web-encoding"
  , "web-file"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
