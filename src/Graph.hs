-- | Graph functions intended for use with MapReduce
module Graph ( explodeNode, gatherEdges ) where

import Data.Function
import Data.List
import Data.Maybe

explodeNode :: Maybe Int -> (String, [String]) -> [(String, [String])]
explodeNode _ (key, val:[]) = [(key, [val, "1"]), (val, [key, "1"])]
explodeNode mdepth row@(_, values) =
  let
    edges = parseEdges values
    depth = fromMaybe (1 + maximum (map snd edges)) mdepth
    newrows = [(e1, [e2, show depth]) | (e1, d1) <- edges,
                                        (e2, d2) <- edges,
                                        e1 /= e2 && d1 == 1 && d2 == depth - 1]
  in row : newrows

gatherEdges :: Bool -> (String, [[String]]) -> (String, [[String]])
gatherEdges outputdepth (x, xs) =
  let rawedges = concatMap parseEdges xs
      edges = map head . groupBy ((==) `on` fst) . sort $ rawedges
      formatEdge (e, d) = if outputdepth then [e, show d] else [e]
  in (x, [concatMap formatEdge edges])

parseEdges :: [String] -> [(String, Int)]
parseEdges (n:d:ps) = (n, read d) : parseEdges ps
parseEdges (_:[]) = error "Unable to parse edge"
parseEdges [] = []
