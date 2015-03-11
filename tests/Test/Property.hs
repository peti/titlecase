{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Property where

import           Prelude                      (($), (.), not)
import           Control.Applicative
import qualified Data.List                    as List
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Semigroup
import           Data.Text
import           Data.Text.Titlecase
import           Data.Text.Titlecase.Internal
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Property tests"
  [ sameLength
  , firstWordCapitalized
  , lastWordCapitalized
  , titlecaseIdempotent
  ]

titlecaseUnsafe :: Text -> Text
titlecaseUnsafe = unTitlecase . titlecase

ignoreList :: [Text]
ignoreList = NonEmpty.toList
   $ (unArticle     <$> articles)
  <> (unConjunction <$> conjunctions)
  <> (unPreposition <$> prepositions)

instance Arbitrary Text where
  arbitrary = elements ignoreList

sameLength :: TestTree
sameLength = testProperty "titlecase doesn't change the length" $
  \t -> length (titlecaseUnsafe t) === length t

firstWordCapitalized :: TestTree
firstWordCapitalized = testProperty "First word is always capitalized" $
  let wordsHead = List.head . words in
  \t -> not (null t) ==>
        wordsHead (titlecaseUnsafe t) === toTitle (wordsHead t)

lastWordCapitalized :: TestTree
lastWordCapitalized = testProperty "Last word is always capitalized" $
  let wordsLast = List.last . words in
  \t -> not (null t) ==>
        wordsLast (titlecaseUnsafe t) === toTitle (wordsLast t)

titlecaseIdempotent :: TestTree
titlecaseIdempotent = testProperty "Applying titlecase twice is the same as doing it once" $
  \t -> titlecaseUnsafe (titlecaseUnsafe t) === titlecaseUnsafe t
