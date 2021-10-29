module Main where

import Relude
import Crawler (loadFile, findLabel, Index(..), printSearchResult, search)
import Text.Pretty.Simple (pPrint)
import Data.Graph (reachable)
import Control.Lens (itraverse_, (^.), At (at))

main :: IO ()
main = do
  [filePath] <- getArgs
  index <- loadFile filePath
  traverse_ (printSearchResult index) (search index)
