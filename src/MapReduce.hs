-- Helper functions for doing map reduce.
module MapReduce ( mapper
                 , reducer ) where

import Data.Function
import Data.Functor
import Data.List
import Control.Monad

mapper :: ([String] -> [[String]]) -> IO ()
mapper f = streaming $ concatMap f

reducer :: ([[String]] -> [[String]]) -> IO ()
reducer f = streaming $ concatMap f . groupBy ((==) `on` head)

streaming :: ([[String]] -> [[String]]) -> IO ()
streaming f = do
  input <- map words . lines <$> getContents
  void . mapM (putStrLn . intercalate "\t") $ f input
