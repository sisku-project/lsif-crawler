module Main where

import Relude
import Crawler (loadFile)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  [filePath] <- getArgs
  parsed <- loadFile filePath
  pPrint parsed
