{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGrid where

import Test.HUnit hiding (Path, path)
import Debug.Trace
import qualified Data.Set as S
import Data.List(sort)
import Small
import Orientation
import Grid

-- TODO label tests
main :: IO Counts
main = runTestTT $ TestList [
  -- Points.
  let
    p = point2' 1 2
    pS = "(1,2)"
  in TestCase $ assertEqual "" (pS, p) (show p, (read pS) :: Point' N2')
  , let
    p = point3' 3 4 5
    pS = "(3,4,5)"
  in TestCase $ assertEqual "" (pS, p) (show p, (read pS) :: Point' N3')
  , let
      p = point3' 4 4 4
      p' = shifted (read "X") 10 p
  in TestCase $ assertEqual "" (point3' 14 4 4) p'
  , let
    p = point2' (-2) 33
    p' = point2' (proj' p (read "X")) (proj' p (read "Y"))
  in TestCase $ assertEqual "" p p'
  , let
    cube = productPoints [0, 1] :: [Point' N3']
    cubeSet = S.fromList cube
  in TestCase $ assertEqual "" 8 (S.size cubeSet)
  , let
    origin = head (productPoints [0] :: [Point' N2'])
  in TestCase $ assertEqual "" (point2' 0 0) origin
  -- Directions.
  , let
    dirs = [read "Z", read "X", read "Y"] :: [Dir' N3']
    allDirs = allValues :: [Dir' N3']
  in TestCase $ assertEqual "" allValues (sort dirs)
  -- Paths.
  , let
    p = Path' (point2' 0 1) [
      Oriented (read "X" :: Dir' N2') Plus
      , Oriented (read "Y" :: Dir' N2') Minus
      ]
    pS = "(0,1)X+Y-"
  in TestCase $ assertEqual "" (pS, p) (show p, (read pS) :: Path' N2')
  , let
    p = Path' (point3' 0 0 0) [
      Oriented (read "X" :: Dir' N3') Plus
      , Oriented (read "Z" :: Dir' N3') Minus
      ]
    pS = "(0,0,0)X+Z-"
  in TestCase $ assertEqual "" (pS, p) (show p, (read pS) :: Path' N3')
  -- Edges.
  , let
    paths = ["(0,0)Y+X+", "(1,0)X+"]
    edges = S.fromList [
      edge' (point2' 0 0) (read "Y+")
      , edge' (point2' 1 1) (read "X-")
      , edge' (point2' 1 0) (read "X+")
      ]
  in TestCase $ assertEqual "" edges (fromPaths' paths)
  , let
    lengths = sort $ map (length . pathSteps) $ paths
    pathSteps (Path' p ods) = ods
    paths = toPaths' edges
    edges = S.fromList [
      edge' (point2' 0 0) (read "X+")
      , edge' (point2' 0 2) (read "X+")
      , edge' (point2' 0 0) (read "Y+")      
      , edge' (point2' 0 1) (read "Y+")
      , edge' (point2' 1 0) (read "Y+")
      , edge' (point2' 1 1) (read "Y+")
      , edge' (point2' 2 0) (read "X+")
      ]
  in TestCase $ assertEqual "" [1, 6] lengths
  , let
    e = edge' (point3' 4 15 6) (read "X-")
    e' = shiftedE (read "Y") (-10) e
  in TestCase $ assertEqual "" "(3,5,6)-(4,5,6)" (show e')
  , let
    e0 :: Edge' N3'
    e0 = edge' (point3' 0 1 4) (read "Z+")
    e1 :: Edge' N3'
    e1 = edge' (point3' 1 7 9) (read "X-")
  in TestCase $ assertEqual "" "[(0,1,4)-(0,1,5),(0,7,9)-(1,7,9)]" (show $ sort [e0, e1])
  , let
    ps = points' (edge' (point3' 2 0 1) (read "Y-"))
    expectedPs = S.fromList [point3' 2 0 1, point3' 2 (-1) 1]
  in TestCase $ assertEqual "" expectedPs ps
  -- Typeclasses.
  , let
    isLower :: (Dimension' n) => Point' (n :: Nat) -> Bool
    isLower p = proj' p (read "X") <= 0
  in TestCase $ assertEqual "" True (isLower $ point3' 0 3 9)
  , let
    frame :: forall n.(Sing n, Dimension' n) => [Edge' n]
    frame = map (\d -> edge' origin (Oriented d Plus)) allDirs where
      origin = head (productPoints [0] :: [Point' n])
      allDirs = allValues :: [Dir' n]
    frame2 = frame :: [Edge' N2']
  in TestCase $ assertEqual "" "[(0,0)-(1,0),(0,0)-(0,1)]" (show frame2)
  -- Natural numbers.
  , let
    values = allValues :: [Nat3]
  in TestCase $ assertEqual "" 3 (length values)
  , let
    values = allValues :: [Nat3]
  in TestCase $ assertEqual "" values (sort values)
  , let
    values = allValues :: [Nat3]
  in TestCase $ assertEqual "" (values !! 2) (maxBound :: Nat3)
  , let
    values = allValues :: [Nat3]
    v0 = values !! 0
    v2 = values !! 2
  in TestCase $ assertEqual "" (succ v0) (pred v2)
  -- TODO add grid2
  {-
  , let
    g = grid2 :: 
  in TestCase $ assertEqual ""
-}
  ]
