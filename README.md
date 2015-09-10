# Titlecase Library for Haskell

This library basically offers a rough but mostly working function `titlecase` to
take any String and capitalize it according to English Title Case. While the
Data.Text function `toTitle` simply capitalizes the first letter of every word,
this Data.Text.Titlecase `titlecase` function uses the list of common words to
not capitalize. It then respects the rule of capitalizing the first and last
words regardless.

On Hackage at <https://hackage.haskell.org/package/titlecase>.
