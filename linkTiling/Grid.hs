{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Grid(
  grid2
  
  , Nat1
  , Nat2
  , Nat3
  , Nat4

  , Nat
  , Sing
  , Dimension'
  , Point'
  , Path'(Path')
  , Dir'
  , Edge'
  , edge'
  , points'
  , fromPaths'
  , toPaths'
  , proj'
  , productPoints
  , shifted
  , shiftedE
  -- Dimensions.
  , N2'
  , point2'
  , N3'
  , point3'
) where

import Data.List(intersperse, find, elemIndex)
import Data.Maybe(maybeToList)
import qualified Data.Set as S
import Small
import Orientation

import Data.Proxy

grid2 :: (Eq s0, Bounded s0, Enum s0, Eq s1, Bounded s1, Enum s1) =>
  (s0, s1) -> Dir' N2' -> Maybe (s0, s1)
grid2 (p0, p1) d = case (show d) of
  "X" -> if p0 == maxBound then Nothing else Just (succ p0, p1)
  "Y" -> if p1 == maxBound then Nothing else Just (p0, succ p1)

instance Small Nat1 where
  allValues = [minBound..maxBound]
instance Small Nat2 where
  allValues = [minBound..maxBound]
instance Small Nat3 where
  allValues = [minBound..maxBound]
instance Small Nat4 where
  allValues = [minBound..maxBound]

-- Natural numbers bounded above by a fixed number.
data Nat1 = N1_0 deriving (Show, Eq, Enum, Ord, Bounded)
data Nat2 = N2_0 | N2_1 deriving (Show, Eq, Enum, Ord, Bounded)
data Nat3 = N3_0 | N3_1 | N3_2 deriving (Show, Eq, Enum, Ord, Bounded)
data Nat4 = N4_0 | N4_1 | N4_2 | N4_3 deriving (Show, Eq, Enum, Ord, Bounded)

-- Experiments.

type Point' n = Vec n Int

{-
Make dependent types based on the dimension (probably 2 or 3).
-}

-- Index into a dimension.
type Dir' n = Index n
instance (Dimension' n) => Show (Dir' n) where
  show d = [indexCoord d letters] where
    letters = directionLetters (Proxy :: Proxy n)
instance (Dimension' n) => Read (Dir' n) where
  readsPrec _ string = do
    case string of
      c : string' -> do
        d <- maybeToList $ indexFind letters c
        return (d, string')
      _ -> []
    where
      letters = directionLetters (Proxy :: Proxy n)

{-
instance forall n.(Sing n) => Small (Index n) where
  allValues = go (singNat (Proxy :: Proxy n)) where
    go :: SingNat n -> [Index n]
    go sn = case sn of
      SingNat0 -> []
      SingNatNext sn' -> Index0 : map IndexNext (go sn')
-}
-- TODO move this to Index level
-- TODO get rid of dependency on letters
-- TODO get rid of multiple scans of letters
instance (Dimension' n) => Small (Dir' n) where
  allValues = map read asStrings where
    asStrings = map (\x -> [x]) $ listFromVec letters
    letters = directionLetters (Proxy :: Proxy n)

data Path' :: Nat -> *  where
  Path' :: Point' n -> [Oriented (Dir' n)] -> Path' n
deriving instance Eq (Path' n)
instance (Dimension' n) => Show (Path' n) where
  show (Path' p ods) = (show p) ++ (concat $ map show ods)
instance forall n.(Dimension' n, Sing n) => Read (Path' n) where
  readsPrec _ s = parsedPath' s
parsedPath' :: forall n.(Dimension' n, Sing n) => String -> [(Path' n, String)]
parsedPath' string =
  do
    (p, string') <- readsPrec 0 string
    (ods, string'') <- parsedOds string'
    return (Path' p ods, string'')
  where
    parsedOds string = go string [] where
      go s ods = case readsPrec 0 s of
        [] -> [(ods, s)]
        -- TODO remove ++ for speed
        [(od, s')] -> go s' (ods ++ [od])

data Edge' :: Nat -> * where
  Edge' :: Point' n -> Point' n -> Edge' n
deriving instance Eq (Edge' n)
deriving instance Ord (Edge' n)
instance Show (Edge' n) where
  show (Edge' p0 p1) = (show p0) ++ "-" ++ (show p1)
edge' :: forall n.(Sing n) => Point' n -> Oriented (Dir' n) -> Edge' n
edge' p od = e p (shifted' od p) where
  e p0 p1 = Edge' (min p0 p1) (max p0 p1)
type Shift' n = Vec n Int
shiftedGen' :: Shift' n -> Point' n -> Point' n
shiftedGen' s p = vecZipWith (+) s p
shifted' :: (Sing n) => Oriented (Dir' n) -> Point' n -> Point' n
shifted' (Oriented d o) p = shifted d delta p where
  delta = if o == Plus then 1 else -1
points' :: Edge' n -> S.Set (Point' n)
points' (Edge' p0 p1) = S.fromList [p0, p1]
shiftedE :: (Sing n) => Dir' n -> Int -> Edge' n -> Edge' n
shiftedE d delta (Edge' p0 p1) = Edge' (shifted d delta p0) (shifted d delta p1)
shifted :: (Sing n) => Dir' n -> Int -> Point' n -> Point' n
shifted d delta p = vecZipWith f (unproj d) p where
  f b x = if b then (x + delta) else x

fromPaths' :: forall n.(Dimension' n, Sing n) => [String] -> S.Set (Edge' n)
fromPaths' paths = S.fromList $ concat (map parsedPathEdges paths) where
  parsedPathEdges :: String -> [Edge' n]
  parsedPathEdges s = pathEdges $ read s
  pathEdges :: Path' n -> [Edge' n]
  pathEdges (Path' p ods) = case ods of
    [] -> []
    od : ods' -> (edge' p od) : pathEdges (Path' (shifted' od p) ods')

toPaths' :: forall n.(Sing n, Dimension' n) => S.Set (Edge' n) -> [Path' n]
toPaths' edges = go edges [] where
  go es paths = case S.size es of
    0 -> paths
    _ -> go es' paths' where
      paths' = path : paths
      (es', path) = withPathExtracted es
  withPathExtracted es = (es', Path' point0 dirs) where
    (es', dirs) = withDirsExtracted es point0
    point0 = S.findMin $ points' $ S.findMin es
    -- TODO remove ++ for performance
    withDirsExtracted es p = go es p [] where
      go es p dirs = case find (\d -> S.member (edge' p d) es) allValues of
        Nothing -> (es, dirs)
        Just dir -> go (S.delete (edge' p dir) es) (shifted' dir p) (dirs ++ [dir])

productPoints :: (Sing n) => [Int] -> [Point' n]
productPoints = vecPower

-- Set up dimensions.

class Dimension' (n :: Nat) where
  directionLetters :: Proxy (n :: Nat) -> Vec n Char

type N2' = 'Next ('Next 'Nat0)
instance Sing N2' where
  singNat _ = SingNatNext (SingNatNext SingNat0)
instance Dimension' N2' where
  directionLetters _ = VecNext 'X' (VecNext 'Y' (Vec0))
point2' x0 x1 = VecNext x0 (VecNext x1 (Vec0))

type N3' = 'Next N2'
instance Sing N3' where
  singNat _ = SingNatNext (SingNatNext (SingNatNext SingNat0))
instance Dimension' N3' where
  directionLetters _ = VecNext 'X' (VecNext 'Y' (VecNext 'Z' Vec0))
point3' x0 x1 x2 = VecNext x0 (VecNext x1 (VecNext x2 Vec0))

-- Vectors.
-- See "Dependent Types in Haskell" by Ishii,
-- "Fixed-Length Vector Types in Haskell (an Update for 2017)" by Le.
-- Other approaches: Data.Vector.Fixed, vector-space.

-- TODO try [()]
-- Make the dimension a Peano natural and promote it to a type using DataKinds.
data Nat = Nat0 | Next Nat deriving (Eq, Ord, Show)
fromNat :: Nat -> Int
fromNat n = case n of
  Nat0 -> 0
  Next n' -> 1 + fromNat n'

{-
class DemoteNat (n :: Nat) where
  demoteNat :: Proxy (n :: Nat) -> Nat
-}
-- Add a proxy so we can retrieve the dimension from the type.
class Sing (n :: Nat) where
  singNat :: Proxy (n :: Nat) -> SingNat n
-- Add a parallel singleton type so we can use the dimension.
data SingNat :: Nat -> * where
  SingNat0 :: SingNat 'Nat0
  SingNatNext :: SingNat n -> SingNat ('Next n)

-- Basically an ordinal: Index n has exactly n inhabitants.
data Index :: Nat -> * where
  Index0 :: Index ('Next n)
  IndexNext :: Index n -> Index ('Next n)
deriving instance Eq (Index n)
deriving instance Ord (Index n)
indexCoord :: Index n -> Vec n a -> a
indexCoord d v = case d of
  Index0 -> case v of
    VecNext x _ -> x
  IndexNext d' -> case v of
    VecNext _ v' -> indexCoord d' v'
indexFind :: (Eq a) => forall n.Vec n a -> a -> Maybe (Index n)
indexFind v x = case v of
  Vec0 -> Nothing
  VecNext y v' -> do
    if (x == y) then Just Index0 else IndexNext <$> (indexFind v' x)

data Vec :: Nat -> * -> * where
  Vec0 :: Vec Nat0 a
  VecNext :: a -> Vec n a -> Vec (Next n) a
listFromVec :: Vec n a -> [a]
listFromVec v = case v of
  Vec0 -> []
  VecNext x v' -> x : listFromVec v'
deriving instance (Eq a) => Eq (Vec n a)
deriving instance (Ord a) => Ord (Vec n a)
instance (Show a) => Show (Vec n a) where
  show v = "(" ++ (concat $ intersperse "," (map show $ listFromVec v)) ++ ")"
instance (Sing n, Read a) => Read (Vec n a) where
  readsPrec _ string = parsedVec string
parsedVec :: forall n.forall a. (Sing n, Read a) => String -> [(Vec n a, String)]
parsedVec s = do
    s' <- case s of
      '(' : t -> [t]
      _ -> []
    go sn s'
  where
    sn = singNat (Proxy :: Proxy n)
    go :: forall n.SingNat n -> String -> [(Vec n a, String)]
    go sn s = case sn of
      SingNat0 -> do
        s' <- case s of
          ')' : t -> [t]
          _ -> []
        return (Vec0, s')
      SingNatNext sn' -> do
        (x, s') <- readsPrec 0 s
        s'' <- case sn' of
          SingNat0 -> [s']
          _ -> case s' of
            ',' : t -> [t]
            _ -> []
        (v', end) <- go sn' s''
        return (VecNext x v', end)
proj' :: Vec n a -> Index n -> a
proj' p d = indexCoord d p
unproj :: forall n.(Sing n) => Index n -> Vec n Bool
unproj d = go sn (Just d) where
  sn = singNat (Proxy :: Proxy n)
  go :: forall n.SingNat n -> Maybe (Index n) -> Vec n Bool
  go sn md = case (sn, md) of
    (SingNat0, _) -> Vec0
    (SingNatNext sn', Nothing) -> VecNext False (go sn' Nothing)
    (SingNatNext sn', Just Index0) -> VecNext True (go sn' Nothing)
    (SingNatNext sn', Just (IndexNext d')) -> VecNext False (go sn' (Just d'))
vecZipWith :: (a0 -> a1 -> a2) -> Vec n a0 -> Vec n a1 -> Vec n a2
vecZipWith f v0 v1 = case (v0, v1) of
  (Vec0, Vec0) -> Vec0
  (VecNext x0 v0', VecNext x1 v1') -> VecNext (f x0 x1) (vecZipWith f v0' v1')
vecMap f v = vecZipWith (\x0 _ -> f x0) v v
-- TODO generalize from const to Dir' -> a
vecPower :: forall n.forall a.(Sing n) => [a] -> [Vec n a]
vecPower xs = go (singNat (Proxy :: Proxy n)) where
  go :: forall n.SingNat n -> [Vec n a]
  go sn = case sn of
    SingNat0 -> [Vec0]
    SingNatNext sn' -> [VecNext x v' | x <- xs, v' <- go sn']
