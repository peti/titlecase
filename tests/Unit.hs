module Unit where

import Data.Text.Titlecase
import qualified Data.Text.Titlecase.Internal as Titlecase
import Data.Text.Titlecase.Internal hiding (articles, conjunctions, prepositions)

import Test.Tasty
import Test.Tasty.HUnit
import Data.Char ( toUpper )

tests :: TestTree
tests = testGroup "Unit tests" [articles, conjunctions, prepositions, issue4, issue5, issue7, examples]

issue5 :: TestTree
issue5 = testGroup "#5: hyphenated titles"
  [ testCase label $ titlecase input @?= expected
  | (label, input, expected) <-
      [ ("reported example", "John wins follow-up election", "John Wins Follow-up Election")
      , ("final up", "a follow-up", "A Follow-Up")
      , ("internal exceptions", "state-of-the-art design", "State-of-the-Art Design")
      , ("mother-in-law", "mother-in-law", "Mother-in-Law")
      , ("final component", "a walk-in", "A Walk-In")
      , ("internal final component", "a walk-in closet", "A Walk-in Closet")
      , ("existing case", "non-English text", "Non-English Text")
      , ("conjunction", "a pick-and-mix selection", "A Pick-and-Mix Selection")
      , ("first component", "the-art of writing", "The-Art of Writing")
      , ("multi-word preposition", "in-front-of", "In-front-Of")
      , ("four-word preposition", "at-the-behest-of", "At-the-behest-Of")
      , ("mixed separators", "  far-from perfect:\tthings to complain-about ", "  Far-from Perfect:\tThings to Complain-About ")
      , ("repeated and boundary hyphens", "--the--story-of--", "--The--Story-Of--")
      , ("hyphens only", " - --\t- ", " - --\t- ")
      , ("Unicode components", "vis-à-vis", "Vis-À-Vis")
      , ("en dash is unchanged", "hello–world", "Hello–world")
      , ("em dash is unchanged", "hello—world", "Hello—world")
      ]
  ]

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
-- Hyphenated entries are governed by component rules, tested in issue5.
prepositions = exceptionTests "Prepositions" (filter (notElem '-') (map unPreposition Titlecase.prepositions))

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
