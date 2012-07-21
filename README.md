SC Data Challenge
=================

Challenge is described here: https://gist.github.com/688e80bb173ee6fb077b

I decided to go with MapReduce using Hadoop's Streaming API, and coded the mapper and reducer executables in Haskell.

How to Use:
-----------

There's a single executable which can be built and installed with cabal.

###Mapper###

Run the mapper by calling

`sc-challenge explodeNode`

You can optionally pass a depth to the mapper

`sc-challenge explodeNode 3`

###Reducer###

Run the reducer by calling

`sc-challenge gatherEdges`

By default this will include depth numbers in the output, if you don't want this call

`sc-challenge gatherEdges False`

###Putting Together###

A MapReduce is needed to calculate each level of depth. Here's how you can try it out with lots of piping:

####N = 1####
`cat input_file | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges False`

####N = 2####
`cat input_file | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges False`

####N = 3####
`cat input_file | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges | sc-challenge explodeNode | sort -k1,1 | sc-challenge gatherEdges False`

The mapper has some special handling built in for simple pair inputs, but apart from this it will expect the input to have depth numbers, so it's only possible to use `gatherEdges False` in the ultimate iteration.

###Tests###

There are some tests. They aren't hooked up to cabal, but you can run in ghci like this:

    Prelude> :cd src
    Prelude> :load Test
    *Test> runTests

What it does:
-------------

For each input row the mapper will determine people who are linked by the next depth of relationships, and mark these inferred relationships by representing them with new rows.

The reducer then gathers all this information that the mapper produced into a single merged row per person.

###N=1###

For the input row:

    davidbowie omid

The mapper will output the 2 rows:

    davidbowie omid 1
    omid davidbowie 1

This means that `davidbowie` is linked to `omid` by 1 degree, and that `omid` is linked to `davidbowie` by 1 degree.

So say we have the input:

    davidbowie omid
    davidbowie kim

Then then the mapper output will be:

    davidbowie omid 1
    omid davidbowie 1
    davidbowie kim 1
    kim davidbowie 1

The reducer then merges the rows together to get all the relationships within 1 degree per person:

    davidbowie kim 1 omid 1
    kim davidbowie 1
    omid davidbowie 1

The reducer sorts the merged items alphabetically, and it also ensures they are unique.

It's quite easy to see that the method works for `N=1`, as all the relationships are given, it's just a matter of generating the reverse relationship.

###N>1###

Suppose for a particular `N`, we have the correct solution for `N-1`, then the chains of links that join people through `N` degrees will be made up of links that we have already discovered in on `N-1` solution.

This is because, for any people `a` and `b`, such that the degrees between them is `N`, so `d(a,b)=N`, there must exist a `c`, such that `d(c,a)=1` and `d(c,b)=N-1`. It then follows that both `d(c,a)` and `d(c,b)` are < `N`, and so will be included in the solution to `N-1`.

Now suppose that we find all `a`, `b` and `c` in our solution to `N-1` such that `d(c,a)=1` and `d(c,b)=N-1`, then we will know that for all of these `d(a,b)=N`, and since all solutions for `N` can be generated from the solution for `N-1`, we know that this is a complete solution for `N`.

Taking the example input from above:

    davidbowie kim 1 omid 1
    kim davidbowie 1
    omid davidbowie 1

For N=2 the mapper will consider each row key to be `c`, and generate new rows for all items such that `d(c,a)=1` and `d(c,b)=N-1`:

    davidbowie kim 1 omid 1
    kim omid 2
    omid kim 2
    kim davidbowie 1
    omid davidbowie 1

The reducer will then combine rows to give the solution:

    davidbowie kim 1 omid 1
    kim davidbowie 1 omid 2
    omid davidbowie 1 kim 2

As the method works for `N=1`, and it will work for any `N`, where `N-1` is solved, we can see that it will work for all `N>0`.

Performance:
------------

As a new MapReduce cycle is needed for each degree from 1 to N the time complexity is linear with respect to N.

The mapper function itself has O(N^2) complexity with respect to the number of existing links in a row, because it searches through different combinations of pairs. As new links get added, we should see the mapper slow slightly.

The number of rows fed into the mapper function is constant, as there will always be one row per name.

The reducer function has O(N) complexity with respect to both the number of rows it needs to process, and the number of links it needs to combine.

Using sparse matrices is quite an efficient way of storing the data, but the storage needed for this will increase linearly with the number of links discovered.

The amount of data outputted by the mapper will depend on the number of links found. At least, once a row has been generated for a link, it will not be generated again in the future.
