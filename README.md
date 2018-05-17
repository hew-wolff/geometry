# Geometry experiments

This was an attempt to (1) invent an efficient convex-hull algorithm; and (2) improve my Haskell. Research suggests that I more or less came up with the Monotone Chain variant of the Grahan scan. It is less efficient due to the multiple sorts, but it still has the O(n log n) runtime that we want.

````
ghci testConvexHull.hs
main
````

Possible improvements:
- Fix bug: cases of length 0 and 1 may fail.
- Compare performance with other implementations.
- Allow coordinate data types other than Rational.

