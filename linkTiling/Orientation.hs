module Orientation(
  Oriented(Oriented)
  , Orientation(Plus, Minus)
  , reversed
) where

import Small

-- Each direction has a plus and a minus form.
data Orientation = Plus | Minus deriving (Ord, Enum, Bounded, Eq)
instance Show Orientation where
  show o = case o of
    Plus -> "+"
    Minus -> "-"
instance Read Orientation where
  readsPrec _ string =
    if length string == 0
    then []
    else
      case head string of
        '+' -> [(Plus, tail string)]
        '-' -> [(Minus, tail string)]
        otherwise -> []
reversed :: Orientation -> Orientation
reversed o = case o of
  Plus -> Minus
  Minus -> Plus
-- TODO Consider replacing with simple pair.
data Oriented a = Oriented a Orientation deriving (Ord, Bounded, Eq)
instance Read a => Read (Oriented a) where
  readsPrec _ string = do
    (x, string') <- readsPrec 0 string
    (or, string'') <- readsPrec 0 string'
    return (Oriented x or, string'')
instance Show a => Show (Oriented a) where
  show (Oriented x o) = show x ++ show o
instance Small Orientation where
  allValues = [minBound..maxBound]
instance (Small a) => Small (Oriented a) where
  allValues = [Oriented x o | x <- allValues, o <- allValues]
