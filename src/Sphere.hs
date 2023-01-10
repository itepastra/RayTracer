module Sphere (Sphere (..)) where

import Intersection ( Intersection(Intersection) )
import Material (Material)
import Primitives (Primitive (..))
import Ray (Ray (..), mv)
import Vec3 (Point, dot, len2)

data Sphere where
    Sphere :: {center :: Point, radius :: Float, material :: Material} -> Sphere

instance Eq Sphere where
    (==) (Sphere c1 r1 m1) (Sphere c2 r2 m2) = c1 == c2 && r1 == r2 && m1 == m2

instance Primitive Sphere where
    intersect r s
        | d < 0 = Nothing
        | otherwise = Just $ Intersection root norml (direction r) e
      where
        oc = origin r - center s
        a = len2 (direction r)
        b = 2 * dot oc (direction r)
        c = len2 oc - radius s * radius s
        d = b * b - 4 * a * c
        e = (-b - sqrt d) / (2 * a)

        root = mv r e
        norml = (root - center s) / pure (radius s)