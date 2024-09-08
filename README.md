# Subtypes

This library provides generic `inj` and `proj` functions to lift/inject subtypes into types containing them,
or extract subtypes from other types.

It is based on snippets from [`compdata`](https://hackage.haskell.org/package/compdata) and inspired by [`generic-lens`](https://hackage.haskell.org/package/generic-lens).

* IIUC it generally does the same thing as `generic-lens`'s [`Data.Generics.Sum.Subtype`](https://hackage.haskell.org/package/generic-lens-2.2.2.0/docs/Data-Generics-Sum-Subtype.html) module but it seems to work on some cases where `generic-lens` did not work for me
* I based the code on `compdata` because I succeeded in following along with its implementation
* Possibly this can all be replaced with a PR to `generic-lens` to make it handle more cases
