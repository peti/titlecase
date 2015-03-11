module Main where

import           Test.Tasty
import qualified Test.Property as Property
import qualified Test.Unit     as Unit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Property.tests, Unit.tests]
