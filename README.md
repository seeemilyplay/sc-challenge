SC Data Challenge
=========================

Challenge is described here: https://gist.github.com/688e80bb173ee6fb077b

I decided to go with MapReduce using Hadoop's Streaming API, and coded up the mapper and reducer executables in Haskell.

Using:
------

There's a single executable which can be built and installed with cabal.

`sc-challenge explodeNode` runs the mapper

`sc-challenge gatherEdges` runs the reducer

and

`sc-challenge finalize` runs the reducer, but for the last time

A MapReduce is needed for each i-th degree, so you may need to do lots of piping! Here's some examples:

N = 1
`cat input_file | sc-challenge explodeNode | sort -k1,1 | sc-challenge finalize`

N = 2
`cat input_file | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges | sc-challenge explodeNode | sort -k1,1 | sc-challenge finalize`

N = 3
`cat input_file | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges | sc-challenge explodeNode | sort -k1,1 | sc-challenge finalize`

Under the covers explodeNode and finalize are almost the same, they just format their output differently.

There are some tests, but there isn't an automated way of running them. Here's how you run in ghci:

```
Prelude> :load src/Test.hs src/Graph.hs
*Test> runTests
```

What it does:
------

The data that comes out of explodeNode and gatherEdges is formatted like this:

```

brendan  kim  2  omid  2  torsten  1
davidbowie  kim  1  omid  1  ziggy  1  mick  2  torsten  2
```

For each row first item is the person name, and the next items are pairs each describing one of the person's relationships.
The first item in the pair is the related name, and the second item is the degrees between them.
In the example above `brendan` and `torsten` are directly linked, and `brendan` and `kim` are linked through another person.

Mapping:

The mapper function `explodeNode` takes a single row and generates new rows based on it, by infering other relationships.

For example if the distance between `brendan` and `torsten` is 1, and the distance between `brendan` and `kim` is 2, then
we can infer that the shortest distance between `torsten` and `kim` is at most 3, (it could be less).

More generally if the distance between friends A and B is x and the distance between friends A and C is y, then we know that
the shortest distance between B and C is at most (x + y), because to get from B to C you can always go through A.

At each iteration we the mapper only generates new relationships for the next relevant level, so on the 3rd run it's looking
for friends that are a distance of 3 away from each other. This is so that it doesn't cover old ground, and so that it
doesn't create data that is unecessary yet.

Reducing:

The reducing function `gatherEdges` combines all the rows for a single person into one. It does this by combining the other rows.
It also sorts the items while it's at it. When the same relationship occurs more than once, but with different distances, the
shortest distance is used.

The reducing function `finalize` does exactly what `gatherEdges` does, except it doesn't output any distance data.


