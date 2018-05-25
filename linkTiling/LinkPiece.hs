{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
A small neighborhood in a link, considered as a set of edges.
Based on a hypercube and all its neigboring edges.
A "face" is a top face of the hypercube and all its neigboring edges.
It is "normalized" to compare with the opposite face of an adjacent link piece.
An "outer face" is those edges outside the hypercube pointing in that direction.
-}
module LinkPiece(
  LinkPiece
  , center
  , normalizedFace
  , outerFace
  {-
  , printed
-}
  , isReduced
  , isBubbleFree
) where

import Data.List(intersperse, sort, foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Grid
import Orientation
import Small

data LinkPiece :: Nat -> * where
  LinkPiece :: [Edge' n] -> LinkPiece n deriving (Ord, Eq)

-- TODO clarify possible loop in reducing process

-- TODO Try to combine Sing and Dimension'
instance forall n.(Sing n, Dimension' n) => Small (LinkPiece n) where
  allValues = map LinkPiece ess where
    ess = foldl extended centralSubsets centralPoints
    --ess = foldl' extended centralSubsets centralPoints
    --ess = foldl' extended centralSubsets []
    --ess = foldl' extended centralSubsets (productPoints [0])
    --ess = foldr extended centralSubsets centralPoints
    extended partials p = concat (map (subsetExtensionsAtCorner p) partials)
    --centralSubsets = subsets (centralEdges :: [Edge' n])
    centralSubsets = goodCenters
-- TODO remove this test code
--l = allValues :: [LinkPiece N3']
centerIsGood :: (Sing n, Dimension' n) => [Edge' n] -> Bool
centerIsGood es = reduced && extendable where
  reduced = isReduced (LinkPiece es)
  extendable = all (< 3) (M.elems counts) where
    counts = foldr f M.empty es
    f e cs = foldr (\p -> M.insertWith (+) p 1) cs ps where
      ps = S.toList $ points' e
goodCenters :: forall n.(Sing n, Dimension' n) => [[Edge' n]]
goodCenters = filter centerIsGood allCenters where
  allCenters = subsets (centralEdges :: [Edge' n])
instance Show (LinkPiece n) where
  show (LinkPiece es) = "{" ++ printedEs ++ "}"  where
    printedEs = concat $ intersperse "," $ map show (sort es)

center :: (Sing n, Dimension' n) => LinkPiece n -> S.Set (Edge' n)
center (LinkPiece es) = S.intersection (S.fromList es) (S.fromList centralEdges)

normalizedFace :: (Sing n, Dimension' n) => LinkPiece n -> Oriented (Dir' n) -> S.Set (Edge' n)
normalizedFace (LinkPiece es) od = normalized d face where
  face = S.intersection (faceIncidentEdges od) (S.fromList es)
  Oriented d o = od
  normalized d es = shiftedEdges (-(base es)) es where
    base = minimum . map coord . edgePoints . S.toList
    coord point = proj' point d
    edgePoints = S.toList . foldr S.union S.empty . map points'
    shiftedEdges delta = S.fromList . map (shiftedE d delta) . S.toList

outerFace :: (Dimension' n, Sing n) => LinkPiece n -> Oriented (Dir' n) -> S.Set (Edge' n)
outerFace (LinkPiece es) od = S.intersection fullOuterFace (S.fromList es) where
  fullOuterFace = S.fromList $ map (\p -> edge' p od) (centralFacePoints od)

{-
-- The full grid looks like this:
-- _|_|_
-- _|_|_
--  | |
printed :: LinkPiece -> String
printed tile = concat (zipWith (++) (printedLines tile) (repeat "\n")) where
  printedLines (LinkPiece es) = map row [0..2] where
    row i = map col [0..4] where
      col j = if e `elem` es then char else ' ' where
        e = edge2 p (Oriented dir Plus)
        p = point2 x (1 - i)
        (char, dir, x) = 
          if even j
          then ('_', read "X" :: Dir2, j `div` 2 - 1)
          else ('|', read "Y" :: Dir2, (j - 1) `div` 2)
-}

-- TODO test predicates in 3 dimensions
isReduced :: (Sing n, Dimension' n) => LinkPiece n -> Bool
isReduced lp = all squareIsReduced centralSquares where
  squareIsReduced square = not hasUTurn && not hasHollowL where
    c = S.intersection (center lp) square
    hasUTurn = S.size c == 3
    hasHollowL = hasL && oppositePointIsEmpty && not lInOrder where
      hasL = S.size c == 2 && S.size meetPoints == 1

      meetPoints = intersectionEdgePoints c where
        intersectionEdgePoints = S.foldr S.intersection centerPoints . S.map points'
      oppositePoint = S.findMin oppositePoints where
        oppositePoints = centerPoints S.\\ (edgePoints c)

      oppositePointIsEmpty = not $ S.member oppositePoint outerEdgePoints where
        outerEdgePoints = edgePoints outerEdges
        outerEdges = foldr S.union S.empty (map (outerFace lp) allValues)
      lInOrder = meetPoint < oppositePoint where
        meetPoint = S.findMin $ meetPoints
      centerPoints = edgePoints square
      edgePoints = S.foldr S.union S.empty . S.map points'

isBubbleFree :: (Sing n, Dimension' n) => LinkPiece n -> Bool
isBubbleFree lp = all tfIsBubbleFree centralSquares where
  tfIsBubbleFree tf = not $ S.isSubsetOf tf c
  c = center lp

subsetExtensionsAtCorner :: (Sing n, Dimension' n) => Point' n -> [Edge' n] -> [[Edge' n]]
subsetExtensionsAtCorner p es = map (es ++) cornerSubsets where
  cornerSubsets = filter (\s -> (length s) `elem` goodSizes) allCornerSubsets
  allCornerSubsets = subsets faceEdgesAtCorner
  goodSizes = case subsetIncidence of
    0 -> [0, 2]
    1 -> [1]
    2 -> [0]
    _ -> []
  faceEdgesAtCorner = map (edge' p) dirs where
    dirs = map f allValues
    f d = Oriented d o where
      o = reversed (toEnum (proj' p d))
  subsetIncidence = length $ filter (\e -> S.member p (points' e)) es

faceIncidentEdges :: (Sing n, Dimension' n) => Oriented (Dir' n) -> S.Set (Edge' n)
faceIncidentEdges = S.fromList . concat . map neighborEdges . centralFacePoints where
  neighborEdges p = map (edge' p) allValues

centralFacePoints :: (Sing n) => Oriented (Dir' n) -> [Point' n]
centralFacePoints (Oriented d o) = filter inFace centralPoints where
  inFace p = level == target where
    target = fromEnum (reversed o)
    level = proj' p d

-- TODO consider tying this into centralEdges
centralSquares :: (Sing n, Dimension' n) => [S.Set (Edge' n)]
centralSquares = concat $ map f coordPairs where
  f (d0, d1) = map (squareFrom (d0, d1)) matchingPoints where
    matchingPoints = filter isMatch centralPoints
    isMatch p = (proj' p d0 == 0) && (proj' p d1 == 0)
    squareFrom (d0, d1) p = S.fromList [
      edge' p od0
      , edge' p od1
      , edge' (shifted d0 1 p) od1
      , edge' (shifted d1 1 p) od0
      ] where
      (od0, od1) = (plus d0, plus d1)
      plus d = Oriented d Plus
  coordPairs = pairs allValues where
    pairs xs = case xs of
      [] -> []
      x : xs' -> [(x, y) | y <- xs'] ++ (pairs xs')

centralEdges :: (Sing n, Dimension' n) => [Edge' n]
centralEdges = result where
  result = concat (map edgesFrom centralPoints)
  edgesFrom p = map (\d -> edge' p (Oriented d Plus)) (goodDirs p)
  goodDirs p = filter (isGood p) allValues
  isGood p d = (proj' p d) == 0

centralPoints :: (Sing n) => [Point' n]
centralPoints = productPoints [0, 1]

subsets xs = case xs of
  [] -> [[]]
  x : xs' -> concat [[xs', x : xs'] | xs' <- subsets xs']
