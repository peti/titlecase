{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | As the name implies, this module is meant to be used only if
-- you want to get access to the internals, say, if you're unhappy
-- with the provided 'Data.Text.Titlecase.titlecase' function.
-- "Data.Text.Titlecase.Internal" doesn't prevent you from creating
-- improperly capitalized 'Titlecase' values.  In any other case,
-- "Data.Text.Titlecase" is what you're looking for.

module Data.Text.Titlecase.Internal where

import           Prelude             (Eq, Show, Bool, ($), (.), uncurry)
import           Control.Applicative
import           Data.Foldable       (elem)
import           Data.List.NonEmpty  hiding (unwords)
import           Data.Semigroup
import           Data.Text
import           Text.Blaze

-- * Types

newtype Titlecase = Titlecase { unTitlecase :: Text } deriving (Eq, Show)

instance ToMarkup Titlecase where
  toMarkup           (Titlecase t) = toMarkup t
  preEscapedToMarkup (Titlecase t) = preEscapedToMarkup t

newtype Article = Article { unArticle :: Text } deriving (Eq, Show)

newtype Conjunction = Conjunction { unConjunction :: Text } deriving (Eq, Show)

data Preposition = OneWordPreposition   Text
                 | TwoWordPreposition   Text Text
                 | ThreeWordPreposition Text Text Text
                 | FourWordPreposition  Text Text Text Text
                 deriving (Eq, Show)


-- * Helpers

(<#>) :: Text -> Text -> Text
x <#> "" = x
x <#> y  = x <> " " <> y

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a,b,c,d) = f a b c d

isElem :: (a -> Text) -> NonEmpty a -> Text -> Bool
isElem f xs = (`elem` (f <$> xs)) . toLower

isArticle, isConjunction,
  isOneWordPreposition :: Text -> Bool
isTwoWordPreposition   :: Text -> Text -> Bool
isThreeWordPreposition :: Text -> Text -> Text -> Bool
isFourWordPreposition  :: Text -> Text -> Text -> Text -> Bool

isArticle                      = isElem unArticle     articles
isConjunction                  = isElem unConjunction conjunctions
isOneWordPreposition           = isElem unPreposition oneWordPrepositions
isTwoWordPreposition   a b     = isElem unPreposition twoWordPrepositions   $ unwords [a, b]
isThreeWordPreposition a b c   = isElem unPreposition threeWordPrepositions $ unwords [a, b, c]
isFourWordPreposition  a b c d = isElem unPreposition fourWordPrepositions  $ unwords [a, b, c, d]

unPreposition :: Preposition -> Text
unPreposition p = case p of
  OneWordPreposition   a       -> a
  TwoWordPreposition   a b     -> unwords [a, b]
  ThreeWordPreposition a b c   -> unwords [a, b, c]
  FourWordPreposition  a b c d -> unwords [a, b, c, d]


-- * Words that are capitalized only when they start or end a title

articles :: NonEmpty Article
articles = Article <$> fromList ["a", "an", "the"]

conjunctions :: NonEmpty Conjunction
conjunctions = Conjunction <$> fromList
  ["for", "and", "nor", "but", "or", "yet", "so"]

prepositions :: NonEmpty Preposition
prepositions
   = oneWordPrepositions
  <> twoWordPrepositions
  <> threeWordPrepositions
  <> fourWordPrepositions

oneWordPrepositions :: NonEmpty Preposition
oneWordPrepositions = OneWordPreposition <$> fromList
  [ "a", "abaft", "abeam", "aboard", "about", "above", "absent", "across"
  , "afore", "after", "against", "along", "alongside", "amid", "amidst"
  , "among", "amongst", "an", "anenst", "apropos", "apud", "around", "as"
  , "aside", "astride", "at", "athwart", "atop"
  , "barring", "before", "behind", "below", "beneath", "beside", "besides"
  , "between", "beyond", "but", "by"
  , "chez", "circa", "concerning", "considering"
  , "despite", "down", "during"
  , "except", "excluding", "failing", "following", "for", "forenenst", "from"
  , "given"
  , "in", "including", "inside", "into"
  , "like"
  , "mid", "midst", "minus", "modulo"
  , "near", "next", "notwithstanding"
  , "of", "off", "on", "onto", "opposite", "out", "outside", "over"
  , "pace", "past", "per", "plus", "pro"
  , "qua"
  , "regarding", "round"
  , "sans", "save", "since", "than", "through"
  ]

twoWordPrepositions :: NonEmpty Preposition
twoWordPrepositions = uncurry TwoWordPreposition <$> fromList
  [ ("according", "to"), ("ahead", "of"), ("apart", "from"), ("as", "for")
  , ("as", "of"), ("as", "per"), ("as", "regards"), ("aside", "from")
  , ("astern", "of")
  , ("back", "to"), ("because", "of")
  , ("close", "to")
  , ("due", "to")
  , ("except", "for")
  , ("far", "from")
  , ("in", "to"), ("inside", "of"), ("instead", "of")
  , ("left", "of")
  , ("near", "to"), ("next", "to")
  , ("on", "to"), ("opposite", "of"), ("opposite", "to"), ("out", "from")
  , ("out", "of"), ("outside", "of"), ("owing", "to")
  , ("prior", "to"), ("pursuant", "to")
  , ("rather", "than"), ("regardless", "of"), ("right", "of")
  , ("subsequent", "to"), ("such", "as")
  , ("thanks", "to"), ("that", "of")
  , ("up", "to")
  ]

threeWordPrepositions :: NonEmpty Preposition
threeWordPrepositions = uncurry3 ThreeWordPreposition <$> fromList
  [ ("as", "far", "as"), ("as", "long", "as"), ("as", "opposed", "to")
  , ("as", "soon", "as"), ("as", "well", "as")
  , ("by", "means", "of"), ("by", "virtue", "of")
  , ("in", "accordance", "with"), ("in", "addition", "to")
  , ("in", "case", "of"), ("in", "front", "of"), ("in", "lieu", "of")
  , ("in", "order", "to"), ("in", "place", "of"), ("in", "point", "of")
  , ("in", "spite", "of")
  , ("on", "account", "of"), ("on", "behalf", "of"), ("on", "top", "of")
  , ("with", "regard", "to"), ("with", "respect", "to")
  ]

fourWordPrepositions :: NonEmpty Preposition
fourWordPrepositions = uncurry4 FourWordPreposition <$> fromList
  [ ("at", "the", "behest", "of")
  , ("for", "the", "sake", "of")
  , ("with", "a", "view", "to")
  ]
