{-# LANGUAGE MultiWayIf #-}

module Data.Text.Titlecase ( titlecase ) where

import Data.Text.Titlecase.Internal
import Data.Char ( isSpace )

-- | Capitalize all English words except 'articles' (a, an, the), coordinating
-- 'conjunctions' (for, and, nor, but, or, yet, so), and 'prepositions' (unless
-- they begin or end the title).
-- ASCII hyphens separate components, which follow the same rules as words.
-- Whitespace and hyphens are preserved exactly.

titlecase :: String -> String
titlecase t = restoreSeparators t (words (go 1 ws))
  where
    -- The parser changes capitalization, but never the number of words.
    -- Put its output words back between the original separator runs.
    restoreSeparators :: String -> [String] -> String
    restoreSeparators original [] = original
    restoreSeparators original (w:rest) =
      let (spacing, text) = span isSeparator original
          remaining = dropWhile (not . isSeparator) text
      in spacing ++ w ++ restoreSeparators remaining rest

    ws :: [String]
    ws        = words (map (\c -> if c == '-' then ' ' else c) t)
    isSeparator c = isSpace c || c == '-'
    isFirst i = i == 1

    go :: Int -> [String] -> String
    go _ []           = ""
    go i (a:b:c:d:tt) = parse4 i a b c d tt
    go i (a:b:c  :tt) = parse3 i a b c   tt
    go i (a:b    :tt) = parse2 i a b     tt
    go i (a      :tt) = parse1 i a       tt

    parse4 i a b c d tt =
      if isFourWordPreposition a b c d
      then if | isFirst i && null tt
                             -> unwords [toTitle a, b, c, toTitle d]
              | isFirst   i  -> unwords [toTitle a, b, c,         d] <#> go (succ i) tt
              | null tt      -> unwords [        a, b, c, toTitle d]
              | otherwise    -> unwords [        a, b, c,         d] <#> go (succ i) tt
      else parse3 i a b c (d:tt)

    parse3 i a b c tt =
      if isThreeWordPreposition a b c
      then if | isFirst i && null tt
                             -> unwords [toTitle a, b, toTitle c]
              | isFirst   i  -> unwords [toTitle a, b,         c] <#> go (succ i) tt
              | null tt      -> unwords [        a, b, toTitle c]
              | otherwise    -> unwords [        a, b,         c] <#> go (succ i) tt
      else parse2 i a b (c:tt)

    parse2 i a b tt =
      if isTwoWordPreposition a b
      then if | isFirst i && null tt
                             -> unwords [toTitle a, toTitle b]
              | isFirst   i  -> unwords [toTitle a,         b] <#> go (succ i) tt
              | null tt      -> unwords [        a, toTitle b]
              | otherwise    -> unwords [        a,         b] <#> go (succ i) tt
      else parse1 i a (b:tt)

    parse1 i a tt =
      if isOneWordPreposition a || isConjunction a || isArticle a
      then if isFirst i || null tt
           then toTitle a <#> go (succ i) tt
           else         a <#> go (succ i) tt
      else      toTitle a <#> go (succ i) tt
