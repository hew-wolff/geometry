# Link tilings (snapshot 2018-05)

I was looking for an efficient way to search for [discrete knots](http://hewwolff.org/DiscreteKnots/), or more generally for links with multiple components. The basic idea was to look at small chunks of the link ("link pieces") as three-dimensional tiles, and find all possible tilings with these tiles. I wanted to test the algorithms in two dimensions (where things are easier to understand) as well as three, and to use the same codebase for both. Implementing fixed-length vectors in Haskell led to language extensions like DataKinds and GADTs.

For example:
````
ghci TestGrid.hs
main
````

