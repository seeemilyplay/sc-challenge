-- | Helper functions for doing map reduce.
module MapReduce ( mapper
                 , reducer ) where

import Control.Arrow
import Data.Function
import Data.List

{- | Turns a function into a mapper function by doing grunt work
     for it, such as dealing with the fact the first item in a row
     is a primary key.
-}
mapper :: ((a, [a]) -> [(b, [b])]) -> [[a]] -> [[b]]
mapper f = map (uncurry (:)) . concatMap f . map (head &&& tail)

{- | Turns a function into a reducer function by doing grunt work
     for it, such as grouping rows together by primary key.
-}
reducer :: Eq a => ((a, [[a]]) -> (b, [[b]])) -> [[a]] -> [[b]]
reducer f =
  concatMap (\(x,xs) -> map ((:) x) xs) . map (f . ((head . head) &&& (map tail))) . groupBy ((==) `on` head)
