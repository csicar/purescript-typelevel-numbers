Typelevel Numbers represented as Symbols
========================================

The library is based on bodil's [typelevel](https://github.com/bodil/purescript-typelevel/) library, but updated for using the new typelevel-prelude classes for `Boolean` and `Kind`.

### Examples 
```purescript
> import Type.Data.Int
> a = sumInt (IProxy::_ "-20") (IProxy::_ "13")
> :t a
IProxy "-7"

```

### Installation

add to spago:
```dhall
let additions = 
	{ typelevel-numbers =
		{ dependencies =
			[ "console"
			, "effect"
			, "partial"
			, "psci-support"
			, "test-unit"
			, "tuples"
			, "typelevel-prelude"
			, "unsafe-coerce"
			]
		, repo = "https://github.com/csicar/purescript-typelevel-numbers"
		, version = "master"
		}
	}
```