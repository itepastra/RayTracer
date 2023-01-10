module Primitives (Primitive (..)) where

import Intersection (Intersection (..))
import Material (Material)
import Ray (Ray (..), mv)
import Vec3 (Point, dot, len2)
import Data.Foldable (minimumBy)

-- class Primitive a where
--     intersect :: Ray -> a -> Maybe Intersection

-- instance (Foldable f, Functor f, Primitive a) => Primitive (f a) where
--     intersect r f = minimumBy p $ fmap (intersect r) f
--       where
--         p Nothing Nothing = EQ
--         p (Just _) Nothing = LT
--         p Nothing (Just _) = GT
--         p (Just a) (Just b) = compare a b

data Primitive = Primitive {shape :: Shape, material :: Material}

data Shape = Sphere {center :: Point, radius :: Float}

intersect :: Ray -> Primitive -> Maybe Intersection
intersect r (Primitive (Sphere c rs) m)
    | discriminant < 0 = Nothing
    | otherwise = Just $ Intersection root norml (direction r) res m
  where
    oc = origin r - c
    a = len2 (direction r)
    half_b = 2 * dot oc (direction r)
    lcd = len2 oc - rs * rs
    discriminant = half_b * half_b - a * lcd

    res = (-half_b - sqrt discriminant) / a
    root = mv r res
    norml = (root - c) / pure rs

intersectf :: (Foldable f, Functor f) => Ray -> f Primitive -> Maybe Intersection
intersectf r f = minimumBy p $ fmap (intersect r) f
      where
        p Nothing Nothing = EQ
        p (Just _) Nothing = LT
        p Nothing (Just _) = GT
        p (Just a) (Just b) = compare a b