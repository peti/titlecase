module Main where

import Test.Tasty
import qualified Test.Property as Property

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Property.tests]
