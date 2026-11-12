# Titlecase Library for Haskell

[![hackage release](https://img.shields.io/hackage/v/titlecase.svg?label=hackage)](http://hackage.haskell.org/package/titlecase)
[![stackage LTS package](http://stackage.org/package/titlecase/badge/lts)](http://stackage.org/lts/package/titlecase)
[![stackage Nightly package](http://stackage.org/package/titlecase/badge/nightly)](http://stackage.org/nightly/package/titlecase)
[![CI Status](https://github.com/peti/titlecase/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/peti/titlecase/actions/workflows/haskell-ci.yml)

This library offers fairly robust `titlecase` function to capitalize a string
to English Title Case. Unlike `toTitle` from the `text` library -- which simply
capitalizes the first letter of every word -- this function respects rules with
regard to articles, conjunctions, and prepositions.

ASCII hyphens separate components that follow the same capitalization rules
as words. For example, `state-of-the-art design` becomes
`State-of-the-Art Design`, and `a walk-in` becomes `A Walk-In` because the
final component ends the title. Internal prepositions stay lowercase:
`follow-up election` becomes `Follow-up Election`. Existing capitalization,
whitespace, and hyphens are preserved. En and em dashes are not treated as
component separators.

It is available from Hackage at <https://hackage.haskell.org/package/titlecase>.
