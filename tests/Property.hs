module Property where

import Data.Text.Titlecase
import Data.Text.Titlecase.Internal

import Data.List as List
import Data.Semigroup
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Property tests"
  [ sameLength
  , firstWordCapitalized
  , lastWordCapitalized
  , titlecaseIdempotent
  ]

ignoreList :: [String]
ignoreList =
     (unArticle     <$> articles)
  <> (unConjunction <$> conjunctions)
  <> (unPreposition <$> prepositions)

arbitraryText :: Gen String
arbitraryText = elements ignoreList

sameLength :: TestTree
sameLength = testProperty "titlecase doesn't change the length" $
  forAll arbitraryText $ \t -> length (titlecase t) === length t

firstWordCapitalized :: TestTree
firstWordCapitalized = testProperty "First word is always capitalized" $
  let wordsHead = List.head . words in
  forAll arbitraryText $
    \t -> not (null t) ==> wordsHead (titlecase t) === toTitle (wordsHead t)

lastWordCapitalized :: TestTree
lastWordCapitalized = testProperty "Last word is always capitalized" $
  let wordsLast = List.last . words in
  forAll arbitraryText $
    \t -> not (null t) ==> wordsLast (titlecase t) === toTitle (wordsLast t)

titlecaseIdempotent :: TestTree
titlecaseIdempotent = testProperty "Applying titlecase twice is the same as doing it once" $
  forAll arbitraryText $ \t -> titlecase (titlecase t) === titlecase t
