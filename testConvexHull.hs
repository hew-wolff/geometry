{-# LANGUAGE OverloadedStrings #-}

module TestConvexHull where

import Test.HUnit
import ConvexHull

main :: IO Counts
main = runTestTT tests
  where
    tests = TestList [
        TestLabel "small" smallTest
        , TestLabel "large" largeTest
      ]

{-
Take a single random list of several nearby integer points.
Then look at the convex hull of the initial segments of this list.
This includes edge cases like only two points, or collinear points.
-}
smallTest :: Test
smallTest = let
  testCase i expected = TestCase (assertEqual ("taking " ++ show i ++ " points") expected (convexHull (sample i)))
  sample i = take i points
  {-
  The points, labeled 0 through 7.
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
  in TestList [
    testCase 2 [P 1 1, P 2 3]
    , testCase 3 [P 1 1, P 1 2, P 2 3]
    , testCase 4 [P 1 1, P 1 2, P 2 3, P 3 0]
    , testCase 5 [P 1 1, P 1 2, P 2 3, P 3 3, P 3 0]
    , testCase 6 [P 1 1, P 1 2, P 2 3, P 3 3, P 3 0]
    , testCase 7 [P 0 1, P 1 2, P 2 3, P 3 3, P 3 0]
    , testCase 8 [P 0 1, P 1 2, P 2 3, P 3 3, P 3 0, P 2 0]
    ]

{-
Take ten thousand points scattered in the unit circle.
The hull points should all be close to the circle.
Also check that the test runs reasonably fast.
-}
largeTest :: Test
largeTest = let
  circlePointSize = 100
  stepSize = (1 / circlePointSize) :: Float
  radiuses = [0, stepSize..1]
  angles = [0, stepSize..(2*pi)]
  polarPoints = [(r, angle) | r <- radiuses, angle <- angles]
  floatPoints = map (\(r, a) -> (r * (cos a), r * (sin a))) polarPoints
  circlePoints = map (\(x, y) -> P (toRational x) (toRational y)) floatPoints
  circleHull = convexHull circlePoints
  distance (P x y) = sqrt((fromRational x) * (fromRational x) + (fromRational y) * (fromRational y))
  hullRadiuses = map distance circleHull
  radiusesGood = map (\r -> abs (1 - r) <= 0.1) hullRadiuses
  in TestList [
    TestCase (assertBool "radiuses OK" (all id radiusesGood))
    ]
