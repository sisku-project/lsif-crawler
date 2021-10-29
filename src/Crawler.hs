module Crawler where

import Control.Lens (At (at), (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Graph (Graph, Vertex, graphFromEdges)
import Flow ((|>))
import Relude
import qualified Relude.Unsafe as Unsafe

loadFile :: FilePath -> IO Index
loadFile filePath = do
  ls <- lines <$> readFileText filePath
  let (index, _, _) = valuesToIndex $ mapMaybe (decode . encodeUtf8) ls
  pure index

data Index = Index
  { _graph :: Graph,
    _vertexes :: Map Int Value
  }
  deriving stock (Show)

valuesToIndex ::
  [Value] ->
  ( Index,
    Vertex -> (Maybe Value, Int, [Int]),
    Int -> Maybe Vertex
  )
valuesToIndex vs = (Index {_graph = graph, _vertexes = vertexes}, nodeFromVertex, vertexFromKey)
  where
    (graph, nodeFromVertex, vertexFromKey) =
      graphFromEdges $ map (\(outV, inVs) -> (Unsafe.fromJust $ vertexes ^? at outV, outV, inVs)) edges
    edges =
      filter (\x -> x ^? key "type" == Just (String "edge")) vs
        |> map convEdge
    convEdge x =
      let inVs :: [Int] = case (x ^? key "inVs" . _Array, x ^? key "inV") of
            (Just inVs', Nothing) -> mapMaybe (fmap fromInteger . (^? _Integer)) $ toList inVs'
            (Nothing, Just inV) -> toList $ fmap fromInteger $ inV ^? _Integer
            _ -> error "invalid"
          outV :: Int = Unsafe.fromJust $ fmap fromInteger (x ^? key "outV" . _Integer)
       in (outV, inVs)
    vertexes =
      filter (\x -> x ^? key "type" == Just (String "vertex")) vs
        |> map (\x -> (Unsafe.fromJust $ fmap fromInteger $ x ^? key "id" . _Integer, x))
        |> fromList
