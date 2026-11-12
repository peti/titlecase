module Property where

import Data.Text.Titlecase
import Data.Text.Titlecase.Internal

import Data.List as List
import Data.Char ( isSpace )
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Property tests"
  [ sameLength
  , firstWordCapitalized
  , lastWordCapitalized
  , titlecaseIdempotent
  , whitespacePreserved
  ]

ignoreList :: [String]
ignoreList =
     fmap unArticle     articles
  ++ fmap unConjunction conjunctions
  ++ fmap unPreposition prepositions

arbitraryText :: Gen String
arbitraryText = unwords <$> listOf (elements ignoreList)

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

whitespacePreserved :: TestTree
whitespacePreserved = testProperty "Whitespace characters keep their positions" $
  forAllShrink (concat <$> listOf (elements ["foo", "and", "in", "front", "of", " ", "  ", "\t", "\r\n", "\160"])) shrink $ \t ->
    let whitespacePositions = filter (isSpace . snd) . zip [0 :: Int ..]
        result = titlecase t
    in conjoin
      [ length result === length t
      , whitespacePositions result === whitespacePositions t
      , words result === words (titlecase (unwords (words t)))
      , titlecase result === result
      ]
