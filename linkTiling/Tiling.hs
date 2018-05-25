{-# LANGUAGE ScopedTypeVariables #-}

{-
Finds all solutions to a finite tiling problem where we know
the positions and physical shapes of the tiles, but the tile faces
vary and must match.
-}
module Tiling(
  Problem(Problem)
  , Tiling(Tiling)
  , allTilings
  , fromList
  , fromTiling
) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Data.List(intersperse)
import Small
import Orientation

data Problem pos dir tile face = Problem {
    grid :: pos -> dir -> Maybe pos
    , tileOptions :: pos -> [tile]
    , tileFace :: tile -> Oriented dir -> face
  }

newtype Tiling pos tile = Tiling (pos -> tile) deriving (Eq, Ord)

instance (Ord pos, Show pos, Small pos, Show t) => Show (Tiling pos t) where
  show tiling = "Tiling {" ++ (concat $ intersperse "," tileStrings) ++ "}" where
    tileStrings = ((map show) . fromTiling) tiling

allTilings :: (
  Small pos, Ord pos, Show pos,
  Small dir, Ord dir, Show dir,
  Show tile, Ord tile, Small tile,
  Eq face
  ) => Problem pos dir tile face -> [Tiling pos tile]
allTilings (Problem grid options face) = map (Tiling . fromMap) resultMaps where
  resultMaps = foldl extended [M.empty] allValues
  extended partials pos = concat (map extensions partials) where
    extensions partial = map (\t -> M.insert pos t partial) (goodTiles partial)
    goodTiles partial = filter (fits partial) (optionsMap M.! pos)
    fits partial t = all (dpGood partial t) (gWalk M.! pos)
    dpGood partial t (od, mpos1) = case mpos1 of
      Just pos1 -> faceMap M.! (t, od) == faceMap M.! (partial M.! pos1, rev od)
      Nothing -> True
    rev (Oriented t o) = Oriented t (reversed o)
  -- Compile the input functions into maps for speed.
  gWalk = gridWalk grid
  optionsMap = toMap options
  faceMap = toMap (\(t, d) -> face t d)

-- Walks through the positions in order, with connections to previous positions
-- that haven't been checked yet.
-- Sort of a directed spanning tree.
gridWalk :: forall dir. forall pos. (Small pos, Ord pos, Small dir, Ord dir) =>
  (pos -> dir -> Maybe pos) -> M.Map pos [(Oriented dir, Maybe pos)]
gridWalk grid = foldl extended M.empty allValues where
  extended w pos = M.insert pos good w where
    list = zip allOds (map (grid' pos) allOds)
    allOds = allValues :: [Oriented dir]
    seenOrMissing mp = case mp of
      Just p -> M.member p w
      Nothing -> True
    good = filter (seenOrMissing . snd) list
  grid' = orientedGrid grid

orientedGrid :: (Small pos, Ord pos, Ord dir, Small dir) =>
  (pos -> dir -> Maybe pos) -> (pos -> Oriented dir -> Maybe pos)
orientedGrid grid = grid' where
  grid' p od = M.lookup (p, od) gridMap'
  gridMap' = M.fromList allSteps
  allSteps = concat $ map bothDirTypes plusSteps
  plusSteps = [((p0, d), p1) | p0 <- allValues, d <- allValues, p1 <- maybeToList (grid p0 d)]
  bothDirTypes ((p0, d), p1) = [
    ((p0, Oriented d Plus), p1),
    ((p1, Oriented d Minus), p0)
    ]

{-
-- We want to do this fast, but also want to debug it when there's an error.
-- TODO Get rid of the Show.
mapValue :: (Ord a, Show a, Show b) => M.Map a b -> a -> b
mapValue m a = m M.! a
--mapValue m a = trace ("mapValue " ++ show m ++ " " ++ show a ++ " " ++ show (M.member a m)) m M.! a
-}

fromList :: (Small pos, Ord pos, Show pos, Show t) => [t] -> Tiling pos t
fromList = Tiling . fromMap . M.fromList . zip allValues
fromTiling :: (Small pos, Ord pos, Show pos, Show t) => Tiling pos t -> [t]
fromTiling (Tiling f) = (map snd . M.toList . toMap) f
