module Intersection (Intersection (..)) where

import Types (Distance)
import Vec3 (Direction, Normal, Point)
import Material (Material)

data Intersection = Intersection
    { point :: Point
    , normal :: Normal
    , incomingDirection :: Direction
    , distance :: Distance
    , intersectMaterial :: Material
    }

instance Eq Intersection where
    (==) (Intersection p1 n1 i1 d1 m1) (Intersection p2 n2 i2 d2 m2) = p1 == p2 && n1 == n2 && i1 == i2 && d1 == d2 && m1 == m2

instance Ord Intersection where
  compare i1 i2 = compare (distance i1) (distance i2)