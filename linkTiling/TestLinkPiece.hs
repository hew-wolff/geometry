module TestLinkPiece where

import Test.HUnit
import Debug.Trace
import qualified Data.Set as S
import Data.List(sort, intersperse)
import LinkPiece
import Grid
import Small

main :: IO Counts
main = runTestTT tests

-- TODO label tests
tests = TestList [
  let 
    lp = head $ (linkPieces (\lp -> center lp == fullCenter2) :: [LinkPiece N2'])
    string = "{(0,0)-(0,1),(0,0)-(1,0),(0,1)-(1,1),(1,0)-(1,1)}"
  in TestCase $ assertEqual "can print square" string (show lp)
  , let
    lp = (lpFromPaths []) :: LinkPiece N2'
  in TestCase $ assertEqual "can print empty" "{}" (show lp)
  , let
    allLps = (linkPieces $ const True) :: [LinkPiece N2']
  in TestCase $ assertEqual "LPs are distinct" (length allLps) (S.size $ S.fromList allLps)
  , let
    allCenters = S.fromList $ map center ((linkPieces $ const True) :: [LinkPiece N2'])
  in TestCase $ assertEqual "all centers are used" 16 (S.size allCenters)
  , let
    emptyCenterPieces = linkPieces (\lp -> center lp == S.empty) :: [LinkPiece N2']
  in TestCase $ assertEqual "each outer corner filled or not" 16 (length emptyCenterPieces)
  , let
    fullCenterPieces = linkPieces (\lp -> center lp == fullCenter2)
  in TestCase $ assertEqual "full center determines LP" 1 (length fullCenterPieces)
  , let
    result :: [[S.Set (Edge' N2')]]
    result = map normalizedFaces (linkPieces (\lp -> center lp == uShapeCenter))
    uShapeCenter = fromPaths' ["(0,1)Y-X+Y+"]
    uShapeFaces = map (map fromPaths') [
      -- _   _
      --  |_|
      -- 
      [["(0,0)X+Y+X+"], ["(0,1)X+Y-X+"], ["(0,0)Y+X-", "(1,0)Y+X+"], ["(0,1)Y-X+Y+"]]
      --  |  _
      --  |_|
      -- 
      , [["(0,0)X+Y+X+"], ["(1,0)X-Y+Y+"], ["(0,0)Y+Y+", "(1,0)Y+X+"], ["(0,1)Y-X+Y+"]]
      -- _  |
      --  |_|
      -- 
      , [["(0,0)X+Y+Y+"], ["(0,1)X+Y-X+"], ["(0,0)Y+X-", "(1,0)Y+Y+"], ["(0,1)Y-X+Y+"]]
      --  | |
      --  |_|
      -- 
      , [["(0,0)X+Y+Y+"], ["(1,0)X-Y+Y+"], ["(0,0)Y+Y+", "(1,0)Y+Y+"], ["(0,1)Y-X+Y+"]]
      ] :: [[S.Set (Edge' N2')]]
    profiles = sort . map (map printedFace)
  in TestCase $ assertEqual "U faces are right" (profiles uShapeFaces) (profiles result)
  , let
  in TestCase $ assertEqual "L shape is an LP" 1 (length (filter (\lp -> lp == lShape) allValues))
  , let
    fs = map fromPaths' [
      ["(0,0)X+X+", "(1,2)Y-X+"]
      , ["(0,1)X+Y-X+"]
      , ["(0,0)Y+X-", "(1,2)Y-X+"]
      , ["(0,1)Y-X+X+"]
      ] :: [S.Set (Edge' N2')]
    printedFaces = map printedFace
  in TestCase $ assertEqual "L faces are right" (printedFaces fs) (printedFaces (normalizedFaces lShape))
  -- , let
  --   drawnLp =
  --     "_  |_\n" ++
  --     " |_ _\n" ++
  --     "     \n" 
  -- in TestCase $ assertEqual "" drawnLp (printed lShape)
  , let
    lp = lpFromPaths ["(-1,0)X+Y+X+Y-X+"] :: LinkPiece N2'
  in TestCase $ assertEqual "U-turn not reduced" False (isReduced lp)
  , let
    lp = lpFromPaths ["(-1,0)X+X+X+"] :: LinkPiece N2'
  in TestCase $ assertEqual "straight line reduced" True (isReduced lp)
  , let
    lp = lpFromPaths ["(0,2)Y-Y-X+X+"] :: LinkPiece N2'
  in TestCase $ assertEqual "big L reduced" True (isReduced lp)
  , let
    lp = lpFromPaths ["(-1,1)X+X+Y-Y-"] :: LinkPiece N2'
  in TestCase $ assertEqual "upside-down L not reduced" False (isReduced lp)
  , let
    lp = lpFromPaths ["(-1,0)X+Y-", "(-1,1)X+X+Y-Y-"] :: LinkPiece N2'
  in TestCase $ assertEqual "upside-down L with block reduced" True (isReduced lp)
  , let
    lp = lpFromPaths [] :: LinkPiece N3'
  in TestCase $ assertEqual "empty reduced" True (isReduced lp)
  -- TODO for various centers, count all LPs and reduced LPs
  -- TODO show U-turn outside center doesn't count
  , let
    bubbleLp = lpFromPaths ["(0,0)X+Y+X-Y-"] :: LinkPiece N2'
    hasBubble lp = not $ isBubbleFree lp
  in TestCase $ assertEqual "" [bubbleLp] (linkPieces hasBubble)
  {-
  , let
    all = allValues :: [LinkPiece N3']
  in TestCase $ assertEqual "each outer corner of 8 filled 4 ways" (4 ^ 8) (length emptyCenterPieces)
-}
  ]

fullCenter2 :: S.Set (Edge' N2')
fullCenter2 = fromPaths' ["(0,0)X+Y+X-Y-"]

printedFace :: S.Set (Edge' n) -> String
printedFace = bracketed "{" "}" . printedEdges where
  printedEdges = concat . intersperse "," . map show . sort . S.toList
  bracketed prefix suffix s = prefix ++ s ++ suffix

-- _  |_
--  |_ _
--      
lShape = lpFromPaths ["(-1,1)X+Y-X+X+", "(1,2)Y-X+"] :: LinkPiece N2'

lpFromPaths :: (Dimension' n, Sing n) => [String] -> LinkPiece n
lpFromPaths strings = head $ linkPieces f where
  f lp = (lpEdges lp == fromPaths' strings)
  lpEdges lp = S.union (center lp) outerEdges where
    outerEdges = (foldr S.union S.empty (map (outerFace lp) allValues))

linkPieces :: (Sing n, Dimension' n) => (LinkPiece n -> Bool) -> [LinkPiece n]
linkPieces f = filter f allValues

normalizedFaces :: (Sing n, Dimension' n) => LinkPiece n -> [S.Set (Edge' n)]
normalizedFaces lp = map (normalizedFace lp) allValues
