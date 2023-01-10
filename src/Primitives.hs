module Primitives (Primitive (..)) where

import Data.Foldable (minimumBy)
import Intersection (Intersection)
import Ray (Ray)

class Primitive a where
    intersect :: Ray -> a -> Maybe Intersection

instance (Foldable f, Functor f, Primitive a) => Primitive (f a) where
    intersect r f = minimumBy p $ fmap (intersect r) f
      where
        p Nothing Nothing = EQ
        p (Just _) Nothing = LT
        p Nothing (Just _) = GT
        p (Just a) (Just b) = compare a b