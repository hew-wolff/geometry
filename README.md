# Geometry experiments

This was an attempt to (1) invent an efficient convex-hull algorithm; and (2) improve my Haskell.  Research suggests I essentially came up with the Monotone Chain variant of the Grahan scan, in a less efficient form due to the multiple sorts (but with the same asymptotic O(n log n) runtime that we want).

````
ghci testConvexHull.hs
main
````
