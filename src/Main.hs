module Main where

import System.Environment
import System.Exit

import Graph
import MapReduce

main :: IO ()
main = do
  -- this is kind of ugly, could look for a command line parsing package to do a better job
  args <- getArgs
  case args of
   ("explodeNode":[]) -> mapper explodeNode
   ("gatherEdges":[]) -> reducer $ gatherEdges True
   ("finalize":[]) -> reducer $ gatherEdges False
   _ -> do
     putStrLn "Usage: sc-challenge [explodeNode|gatherEdges|finalize]"
     exitFailure
