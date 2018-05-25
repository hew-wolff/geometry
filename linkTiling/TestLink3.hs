{-# LANGUAGE ScopedTypeVariables #-}

module TestLink3 where

import Test.HUnit
import Debug.Trace
import qualified Data.Set as S
import Small
import Orientation
import Grid
import LinkPiece
import Tiling

-- TODO merge with TestLink2
main :: IO Counts
main = runTestTT $ TestList [
  let
    problem = linkProblem basicTileOptions :: LinkProblem (Nat1, Nat1, Nat1)
  in TestCase $ assertEqual "" 1 (length (allTilings problem))
  {-
  , let
    problem = linkProblem basicTileOptions :: LinkProblem (Nat1, Nat2)
  in TestCase $ assertEqual "" 4 (length (allTilings problem))
  , let
    problem = linkProblem basicTileOptions :: LinkProblem (Nat2, Nat2)
  in TestCase $ assertEqual "" 14 (length (allTilings problem))
  , let
    problem = linkProblem reducedTileOptions :: LinkProblem (Nat1, Nat1)
  in TestCase $ assertEqual "" 2 (length (allTilings problem))
  , let
    problem = linkProblem reducedTileOptions :: LinkProblem (Nat1, Nat2)
  in TestCase $ assertEqual "" 3 (length (allTilings problem))
  , let
    problem = linkProblem reducedTileOptions :: LinkProblem (Nat2, Nat2)
  in TestCase $ assertEqual "" 5 (length (allTilings problem))
  , let
    problem = linkProblem reducedTileOptions :: LinkProblem (Nat3, Nat3)
    twoComponentLinks = filter (hasComponents 2) (allTilings problem)
    count = small2 + smallLarge where
      small2 = straight + knight + diagonal where
        straight = 6
        knight = 8
        diagonal = 2
      smallLarge = 1
  in TestCase $ assertEqual "" count (length twoComponentLinks)
  , let
    problem = linkProblem reducedTileOptions :: LinkProblem (Nat3, Nat3)
    knots = filter (hasComponents 1) (allTilings problem)
  in TestCase $ assertEqual "" 9 (length knots)
  , let
    problem = linkProblem reducedBubbleFreeTileOptions :: LinkProblem (Nat3, Nat3)
    emptyTiling = Tiling (const emptyLp)
    emptyLp = head $ filter isEmpty allValues where
      isEmpty lp = S.size (center lp) == 0
  in TestCase $ assertEqual "" [emptyTiling] (allTilings problem)
-}
  ]

type LinkProblem pos = Problem pos (Dir' N3') (LinkPiece N3') (S.Set (Edge' N3'))

grid3 :: (
  Eq s0, Bounded s0, Enum s0
  , Eq s1, Bounded s1, Enum s1
  , Eq s2, Bounded s2, Enum s2) =>
  (s0, s1, s2) -> Dir' N3' -> Maybe (s0, s1, s2)
grid3 (p0, p1, p2) d = case (show d) of
  "X" -> if p0 == maxBound then Nothing else Just (succ p0, p1, p2)
  "Y" -> if p1 == maxBound then Nothing else Just (p0, succ p1, p2)
  "Z" -> if p2 == maxBound then Nothing else Just (p0, p1, succ p2)

linkProblem tileOptions = Problem grid3 tileOptions normalizedFace

basicTileOptions pos = filter (tileOk pos) allValues
reducedTileOptions pos = filter f allValues where
  f lp = (tileOk pos lp) && (isReduced lp)
reducedBubbleFreeTileOptions pos = filter f allValues where
  f lp = (tileOk pos lp) && (isReduced lp) && (isBubbleFree lp)

hasComponents n = (== n) . length . toPaths' . tilingEdges

tileOk (x0, x1, x2) lp = all (faceOk lp) allValues where
  faceOk lp od = (not onBorder) || outerEmpty where
    outerEmpty = (outerFace lp od == S.empty)
    onBorder = case show od of
      "X+" -> x0 == maxBound
      "X-" -> x0 == minBound
      "Y+" -> x1 == maxBound
      "Y-" -> x1 == minBound
      "Z+" -> x2 == maxBound
      "Z-" -> x2 == minBound

tilingEdges (Tiling tilingF) = foldr S.union S.empty edgeSets where
  edgeSets = [lpEdges2 pos (tilingF pos) | pos <- allValues]
  lpEdges2 pos lp = S.map (shiftedEdge2 pos) (center lp)
  shiftedEdge2 (x0, x1, x2) =
    shiftedE (read "X") (fromEnum x0) .
    shiftedE (read "Y") (fromEnum x1) .
    shiftedE (read "Z") (fromEnum x2)
