module Main where

import Crawler (loadFile, search, searchResultToJson)
import Relude
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(Array))

main :: IO ()
main = do
  [filePath] <- getArgs
  index <- loadFile filePath
  putLBS $ Aeson.encode $ Array $ fromList $ map (searchResultToJson index) (search index)
