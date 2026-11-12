module Property where

import Data.Text.Titlecase
import qualified Data.Text.Titlecase.Internal as Titlecase

import Data.Char ( isSpace, toUpper )
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Property tests"
  [ sameLength
  , firstWordCapitalized
  , lastWordCapitalized
  , titlecaseIdempotent
  , whitespacePreserved
  , wordContentsPreserved
  , hyphensPreserved
  , hyphensSeparateComponents
  , whitespaceDoesNotAffectCapitalization
  ]

ignoreList :: [String]
ignoreList =
     fmap Titlecase.unArticle Titlecase.articles
  ++ fmap Titlecase.unConjunction Titlecase.conjunctions
  ++ fmap Titlecase.unPreposition Titlecase.prepositions

-- Include complete multi-word prepositions as well as words outside the
-- exception lists. Mixed case and punctuation exercise preservation rules.
arbitraryText :: Gen String
arbitraryText = frequency
  [ (3, unwords <$> listOf (elements ignoreList))
  , (7, concat <$> listOf (frequency
      [ (4, elements ignoreList)
      , (3, elements ["hello", "world", "Perfect", "iPhone", "NASA", "éclair", "über", "123", "follow-up", "state-of-the-art", "in-front-of", "\"hello\"", "(world)", "story:", "and,"])
      , (3, elements [" ", "  ", "\t", "\n", "\r\n", "\160", "\8195", "\t \n", "-", "--"])
      ]))
  ]

-- Shrinking strings can remove words, shorten them, and simplify separators.
-- All properties below also cover empty and whitespace-only inputs.
forText :: (String -> Property) -> Property
forText = forAllShrink arbitraryText shrink

sameLength :: TestTree
sameLength = testProperty "Titlecase preserves length" $
  forText $ \t -> length (titlecase t) === length t

firstWordCapitalized :: TestTree
firstWordCapitalized = testProperty "First word is always capitalized" $
  forText $ \t -> case (components t, components (titlecase t)) of
    ([], result) -> result === []
    ((c:cs):_, result:_) -> result === toUpper c : cs
    _ -> counterexample "Missing first word" False

lastWordCapitalized :: TestTree
lastWordCapitalized = testProperty "Last word is always capitalized" $
  forText $ \t -> case (reverse (components t), reverse (components (titlecase t))) of
    ([], result) -> result === []
    ((c:cs):_, result:_) -> result === toUpper c : cs
    _ -> counterexample "Missing last word" False

titlecaseIdempotent :: TestTree
titlecaseIdempotent = testProperty "Applying titlecase twice is the same as doing it once" $
  forText $ \t -> titlecase (titlecase t) === titlecase t

whitespacePreserved :: TestTree
whitespacePreserved = testProperty "Whitespace characters keep their positions" $
  forText $ \t ->
    let whitespacePositions = filter (isSpace . snd) . zip [0 :: Int ..]
    in whitespacePositions (titlecase t) === whitespacePositions t

wordContentsPreserved :: TestTree
wordContentsPreserved = testProperty "Only component-initial characters may change" $
  forText $ \t ->
    let original = components t
        result = components (titlecase t)
        unchanged inputWord outputWord = case (inputWord, outputWord) of
          (c:cs, d:ds) -> conjoin [cs === ds, property (d == c || d == toUpper c)]
          _ -> counterexample "Unexpected empty word" False
    in conjoin ((length result === length original) : zipWith unchanged original result)

whitespaceDoesNotAffectCapitalization :: TestTree
whitespaceDoesNotAffectCapitalization = testProperty "Whitespace does not affect capitalization" $
  forText $ \t -> words (titlecase t) === words (titlecase (unwords (words t)))

-- A component ends at either whitespace or an ASCII hyphen.
components :: String -> [String]
components = words . map (\c -> if c == '-' then ' ' else c)

hyphensPreserved :: TestTree
hyphensPreserved = testProperty "Hyphens keep their positions" $
  forText $ \t ->
    let positions = filter ((== '-') . snd) . zip [0 :: Int ..]
    in positions (titlecase t) === positions t

hyphensSeparateComponents :: TestTree
hyphensSeparateComponents = testProperty "Hyphenated components follow word rules" $
  forText $ \t -> components (titlecase t) === words (titlecase (unwords (components t)))
