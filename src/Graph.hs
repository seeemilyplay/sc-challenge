-- Graph functions intended for use with MapReduce
module Graph ( explodeNode, gatherEdges ) where

import Data.Function
import Data.List

explodeNode :: [String] -> [[String]]
explodeNode [] = []
explodeNode (x:y:[]) = [[x, y, "1"], [y, x, "1"]]
explodeNode xs =
  let edges = parseEdges xs
      -- save on record generation if we pass dist in
      dist = 1 + maximum (map snd edges) in
  xs : [[e1, e2, show dist] | (e1, d1) <- edges,
                              (e2, d2) <- edges,
                              e1 /= e2 && d1 + d2 == dist]

gatherEdges :: Bool -> [[String]] -> [[String]]
gatherEdges outputdistances (x:xs) =
  let rawedges = concatMap parseEdges (x:xs)
      edges = map head . groupBy ((==) `on` fst) . sort $ rawedges
      formatEdge (e, d) = if outputdistances then [e, show d] else [e] in
  [head x : concatMap formatEdge edges]
gatherEdges _ [] = []

parseEdges :: [String] -> [(String, Int)]
parseEdges (_:xs) = f xs
  where
    f (n:d:ps) = (n, read d) : f ps
    f (_:[]) = error "Unable to parse edge"
    f [] = []
parseEdges [] = []
