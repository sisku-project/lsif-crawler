{-# LANGUAGE TemplateHaskell #-}

module Crawler where

import Control.Lens (makeLenses, (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Graph.Inductive (Gr, Graph (labNodes), lab, mkGraph, pre, suc)
import Data.Graph.Inductive.Graph (Node)
import Flow ((|>))
import Relude
import qualified Relude.Unsafe as Unsafe

newtype Index = Index
  { _graph :: Gr Value Text
  }
  deriving stock (Show)

makeLenses ''Index

loadFile :: FilePath -> IO Index
loadFile filePath = do
  ls <- lines <$> readFileText filePath
  let index = valuesToIndex $ mapMaybe (decode . encodeUtf8) ls
  pure index

valuesToIndex :: [Value] -> Index
valuesToIndex vs = Index {_graph = mkGraph nodes edges}
  where
    nodes =
      filter (\x -> x ^? key "type" == Just (String "vertex")) vs
        |> map (\x -> (Unsafe.fromJust $ fmap fromInteger $ x ^? key "id" . _Integer, x))
    edges =
      filter (\x -> x ^? key "type" == Just (String "edge")) vs
        |> concatMap convEdge
    convEdge x =
      let inVs :: [Int] = case (x ^? key "inVs" . _Array, x ^? key "inV") of
            (Just inVs', Nothing) -> mapMaybe (fmap fromInteger . (^? _Integer)) $ toList inVs'
            (Nothing, Just inV) -> toList $ fmap fromInteger $ inV ^? _Integer
            _ -> error "invalid"
          outV :: Int = Unsafe.fromJust $ fmap fromInteger (x ^? key "outV" . _Integer)
          label = fromMaybe "noLabel" (x ^? key "label" . _String)
       in map (outV,,label) inVs

data SearchResult = SearchResult
  { hover :: Node,
    definition :: Node,
    defRanges :: [(Node, Node)],
    moniker :: Node
  }
  deriving stock (Show)

search :: Index -> [SearchResult]
search Index {_graph = gr} = map ?? hoverResults $ \hoverResult ->
  executingState SearchResult {hover = hoverResult, definition = 0, defRanges = [], moniker = 0} do
    traverse_ ?? pre gr hoverResult $ \p ->
      traverse_ ?? results p $ \r ->
        case Unsafe.fromJust (lab gr r) ^? key "label" of
          Just (String "definitionResult") -> do
            modify $ \x -> x {definition = r}
            modify $ \x -> x {defRanges = defRanges x <> defRange r}
          Just (String "moniker") -> do
            modify $ \x -> x {moniker = r}
          _ -> pure ()
  where
    hoverResults = map fst $ filter (\(_, v) -> v ^? key "label" == Just (String "hoverResult")) $ labNodes gr
    results i = concatMap ?? suc gr i $ \next ->
      if Unsafe.fromJust (lab gr next) ^? key "label" == Just (String "resultSet")
        then suc gr next
        else [next]
    defRange defNode = do
      range <- suc gr defNode
      definition <- filter (\p -> Unsafe.fromJust (lab gr p) ^? key "label" == Just (String "document")) $ pre gr range
      pure (definition, range)

searchResultToJson :: Index -> SearchResult -> Value
searchResultToJson Index {_graph = gr} SearchResult {..} =
  object
    [ "hover" .= lab gr hover,
      "definition" .= lab gr definition,
      "defRanges" .= map (bimap (lab gr) (lab gr)) defRanges,
      "moniker" .= lab gr moniker
    ]
