module Small(
  Small(allValues)
  , fromMap
  , toMap
) where

import qualified Data.Map.Strict as M

class Small a where
  allValues :: [a]
  
fromMap :: (Small a, Show a, Ord a, Show b) => M.Map a b -> (a -> b)
fromMap = (M.!)

toMap :: (Small a, Ord a) => (a -> b) -> M.Map a b
toMap f = M.fromList $ map ((,) <$> id <*> f) allValues

instance (Small a, Ord a, Eq b) => Eq (a -> b) where
  f0 == f1 = toMap f0 == toMap f1
instance (Small a, Ord a, Show a, Show b) => Show (a -> b) where
  show f = show (toMap f)
instance (Small a0, Small a1) => Small (a0, a1) where
  allValues = [(x0, x1) | x0 <- allValues, x1 <- allValues]
instance (Small a0, Small a1, Small a2) => Small (a0, a1, a2) where
  allValues = [(x0, x1, x2) | x0 <- allValues, x1 <- allValues, x2 <- allValues]

{-
instance (Bounded a0, Bounded a1, Enum a0, Enum a1) => Enum (a0, a1) where
  succ (x0, x1) = if fromEnum x1 == fromEnum (maxBound :: a1)
    then (succ x0, minBound) else (x0, succ x1)
  pred (x0, x1) = if fromEnum x1 == fromEnum (minBound :: a1)
    then (pred x0, maxBound) else (x0, pred x1)
  toEnum i = let
    (d, m) = i `divMod` (fromEnum (maxBound :: a1) + 1)
    in (toEnum d, toEnum m)
  fromEnum (x0, x1) = fromEnum x0 * (fromEnum (maxBound :: a1) + 1) + fromEnum x1
-}

instance (Small a, Ord a, Ord b) => Ord (a -> b) where
  f0 <= f1 = toMap f0 <= toMap f1
