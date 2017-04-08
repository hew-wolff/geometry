# Geometry experiments

Here's an attempt to (1) invent an efficient convex-hull algorithm; and (2) improve my Haskell.  Research suggests it's essentially the Monotone Chain variant of the Grahan scan, although less efficient due to the multiple sorts (but with the same asymptotic O(n log n) runtime that we want).

````
ghci testConvexHull.hs
main
````
