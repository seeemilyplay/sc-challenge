module Test where

import Data.Function
import Data.List
import Data.Ord
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)

import MapReduce
import Graph

runTests = defaultMain tests

tests =
  [ testCase "Example with N=2" testExampleWithN2
  , testCase "Example with N=3" testExampleWithN3
  ]

testExampleWithN2 :: Assertion
testExampleWithN2 =
  let output = mapReduce False $ mapReduce True exampleInput in
  assertEqual "Bad output for example with N=2" (exampleOutput 2) output

testExampleWithN3 :: Assertion
testExampleWithN3 =
  let output = mapReduce False . mapReduce True $ mapReduce True exampleInput in
  assertEqual "Bad output for example with N=3" (exampleOutput 3) output

mapReduce :: Bool -> [[String]] -> [[String]]
mapReduce outputdepth input =
  let mapperout = mapper (explodeNode Nothing) input
      reducerin = sortBy (comparing head) mapperout
  in reducer (gatherEdges outputdepth) reducerin

exampleInput :: [[String]]
exampleInput =
  [ ["davidbowie", "omid"]
  , ["davidbowie", "kim"]
  , ["kim", "torsten"]
  , ["torsten", "omid"]
  , ["brendan", "torsten"]
  , ["ziggy", "davidbowie"]
  , ["mick", "ziggy"]
  ]

exampleOutput :: Int -> [[String]]
exampleOutput 2 =
  [ ["brendan", "kim", "omid", "torsten"]
  , ["davidbowie", "kim", "mick", "omid", "torsten", "ziggy"]
  , ["kim", "brendan", "davidbowie", "omid", "torsten", "ziggy"]
  , ["mick", "davidbowie", "ziggy"]
  , ["omid", "brendan", "davidbowie", "kim", "torsten", "ziggy"]
  , ["torsten", "brendan", "davidbowie", "kim", "omid"]
  , ["ziggy", "davidbowie", "kim", "mick", "omid"]
  ]
exampleOutput 3 =
  [ ["brendan", "davidbowie", "kim", "omid", "torsten"]
  , ["davidbowie", "brendan", "kim", "mick", "omid", "torsten", "ziggy"]
  , ["kim", "brendan", "davidbowie", "mick", "omid", "torsten", "ziggy"]
  , ["mick", "davidbowie", "kim", "omid", "ziggy"]
  , ["omid", "brendan", "davidbowie", "kim", "mick", "torsten", "ziggy"]
  , ["torsten", "brendan", "davidbowie", "kim", "omid", "ziggy"]
  , ["ziggy", "davidbowie", "kim", "mick", "omid", "torsten"]
  ]
exampleOutput _ = error "invalid N"
