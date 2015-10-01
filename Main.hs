module Main ( main ) where

import Data.Text ( pack )
import Data.Text.Titlecase
import System.Environment
import Text.Blaze
import Text.Blaze.Renderer.Pretty

main :: IO ()
main = getArgs >>= putStr . renderMarkup . toMarkup . titlecase . pack . unwords
