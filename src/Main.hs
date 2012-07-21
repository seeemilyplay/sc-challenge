module Main (main) where

import Control.Monad
import Data.Functor
import Data.List
import System.Environment
import System.Exit

import Graph
import MapReduce

main :: IO ()
main = do
  args <- getArgs
  case args of
   ("explodeNode":[]) -> stream $ mapper $ explodeNode Nothing
   ("explodeNode":depth:[]) -> stream $ mapper $ explodeNode (Just $ read depth)
   ("gatherEdges":[]) -> stream $ reducer $ gatherEdges True
   ("gatherEdges":"False":[]) -> stream $ reducer $ gatherEdges False
   _ -> do
     putStrLn "Usage: sc-challenge [explodeNode (<depth>)?|gatherEdges (False)?]"
     exitFailure

stream :: ([[String]] -> [[String]]) -> IO ()
stream f = do
  input <- map words . lines <$> getContents
  void . mapM (putStrLn . intercalate "\t") $ f input
