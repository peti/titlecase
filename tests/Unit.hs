module Unit where

import Data.Text.Titlecase
import qualified Data.Text.Titlecase.Internal as Titlecase
import Data.Text.Titlecase.Internal hiding (articles, conjunctions, prepositions)

import Test.Tasty
import Test.Tasty.HUnit
import Data.Char ( toUpper )

tests :: TestTree
tests = testGroup "Unit tests" [articles, conjunctions, prepositions, issue4, issue7, examples]

issue7 :: TestTree
issue7 = testGroup "#7: preserve whitespace"
  [ testCase label $ titlecase input @?= expected
  | (label, input, expected) <-
      [ ("leading spaces", "  foo bar", "  Foo Bar")
      , ("trailing spaces", "foo bar  ", "Foo Bar  ")
      , ("repeated spaces", "foo  bar", "Foo  Bar")
      , ("tab", "foo\tbar", "Foo\tBar")
      , ("nonbreaking space", "foo\160bar", "Foo\160Bar")
      , ("newlines", "foo\r\nbar\n", "Foo\r\nBar\n")
      , ("empty input", "", "")
      , ("whitespace only", " \t\r\n\160 ", " \t\r\n\160 ")
      , ("padded boundary words", "\tthe story of\n", "\tThe Story Of\n")
      , ("two-word preposition", "\tfar  from perfect about\n", "\tFar  from Perfect About\n")
      , ("three-word preposition", "in\tfront  of", "In\tfront  Of")
      , ("four-word preposition", "at\tthe\nbehest\160of  ", "At\tthe\nbehest\160Of  ")
      , ("internal preposition", "a tale in\tfront of us", "A Tale in\tfront of Us")
      ]
  ]

issue4 :: TestTree
issue4 = testCase "#4: capitalize final word after a multi-word preposition" $
  titlecase "far from perfect: things to complain about"
    @?= "Far from Perfect: Things to Complain About"

-- Each phrase is a separate test so failures identify the exact exception.
articles, conjunctions, prepositions :: TestTree
articles = exceptionTests "Articles" (map unArticle Titlecase.articles)
conjunctions = exceptionTests "Conjunctions" (map unConjunction Titlecase.conjunctions)
prepositions = exceptionTests "Prepositions" (map unPreposition Titlecase.prepositions)

exceptionTests :: String -> [String] -> TestTree
exceptionTests label phrases = testGroup label
  [ testGroup phrase
      [ testCase "first" $
          titlecase (phrase ++ " example") @?= capitalize phrase ++ " Example"
      , testCase "last" $
          titlecase ("example " ++ phrase) @?= "Example " ++ capitalizeLast phrase
      , testCase "internal" $
          titlecase ("example " ++ phrase ++ " example") @?= "Example " ++ phrase ++ " Example"
      , testCase "whole title" $
          titlecase phrase @?= capitalize (capitalizeLast phrase)
      ]
  | phrase <- phrases
  ]
  where
    capitalize "" = ""
    capitalize (c:cs) = toUpper c : cs
    capitalizeLast = unwords . reverse . firstUpper . reverse . words
    firstUpper [] = []
    firstUpper (w:ws) = capitalize w : ws

examples :: TestTree
examples = testGroup "Examples"
  [ testCase label $ titlecase input @?= expected
  | (label, input, expected) <-
      [ ("ordinary words and exceptions", "a journey through the world", "A Journey through the World")
      , ("single article", "the", "The")
      , ("single ordinary word", "hello", "Hello")
      , ("preserve existing case", "an iPhone and NASA", "An IPhone and NASA")
      , ("Unicode initials", "éclair and über", "Éclair and Über")
      , ("digits", "123 stories", "123 Stories")
      , ("punctuation preserved", "hello, world!", "Hello, World!")
      , ("overlapping prepositions", "in front of the house", "In front of the House")
      ]
  ]
