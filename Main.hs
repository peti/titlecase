module Main ( main ) where

import Data.Text.Titlecase
import System.Environment

main :: IO ()
main = getArgs >>= putStrLn . titlecase . unwords
