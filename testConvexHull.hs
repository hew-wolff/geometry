{-# LANGUAGE OverloadedStrings #-}

module TestConvexHull where

import Test.HUnit
import ConvexHull

main :: IO Counts
main = runTestTT tests
  where
    tests = TestList [
        --testCase "1" [P 1 1] (example 1)
        testCase "2" [P 1 1, P 2 3] (example 2)
        , testCase "3" [P 1 1, P 1 2, P 2 3] (example 3)
        , testCase "4" [P 1 1, P 1 2, P 2 3, P 3 0] (example 4)
        , testCase "5" [P 1 1, P 1 2, P 2 3, P 3 3, P 3 0] (example 5)
        , testCase "6" [P 1 1, P 1 2, P 2 3, P 3 3, P 3 0] (example 6)
        , testCase "7" [P 0 1, P 1 2, P 2 3, P 3 3, P 3 0] (example 7)
        , testCase "8" [P 0 1, P 1 2, P 2 3, P 3 3, P 3 0, P 2 0] (example 8)
      ]
    testCase label expected input = TestLabel "test" (TestCase (assertEqual label expected (convexHull input)))
    example i = take i points
    {-
    . . 0 4
    . 2 . .
    6 1 5 .
    . . 7 3
    -}
    points = [
        P 2 3
        , P 1 1
        , P 1 2
        , P 3 0
        , P 3 3
        , P 2 1
        , P 0 1
        , P 2 0
      ]
