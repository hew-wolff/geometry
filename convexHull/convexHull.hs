{-# LANGUAGE OverloadedStrings #-}

module ConvexHull where

import Data.List

data Point = P Rational Rational deriving (Show, Eq, Ord)

leftTurned (P x y) = P (-y) x
rightTurned (P x y) = P y (-x)
slope (P x0 y0) (P x1 y1) = case x1 - x0 of
  0 -> Nothing
  run -> Just ((y1 - y0) / run)

-- Given distinct points, return hull points, clockwise from left.
-- Builds in four "quadrants".
convexHull :: [Point] -> [Point]
convexHull points = let
    go ps qIndex = case qIndex of
      4 -> []
      _ -> (quadHull ps) ++ (map rightTurned (go (map leftTurned ps) (qIndex + 1)))
  in go points 0

quadHull :: [Point] -> [Point]
quadHull ps = let
    sortedPoints = sort ps
    left = head sortedPoints
    top = rightTurned (minimum (map leftTurned ps))
    go remaining lastHull = let
         p : remaining' = remaining
         slopeTo = slope lastHull
         onHull = (slopeTo p == Nothing) || slopeTo p >= slopeTo top
         next = if onHull then p : (go remaining' p) else (go remaining' lastHull)
      in if p == top then [] else next
  in go sortedPoints left
