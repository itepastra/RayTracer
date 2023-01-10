module Vec3 where

import Helpers (merge)

data Vec3 a = Vec3
    { x :: a
    , y :: a
    , z :: a
    }

type Point = Vec3 Float
type Direction = Vec3 Float
type Normal = Vec3 Float

toTup :: Vec3 c -> (c, c, c)
toTup (Vec3 x y z) = (x, y, z)

fromTup :: (a, a, a) -> Vec3 a
fromTup (x, y, z) = Vec3 x y z

scale :: (Functor f, Num b) => b -> f b -> f b
scale f = fmap (f *)

dot :: (Foldable t, Num a, Applicative t) => t a -> t a -> a
dot v1 = sum . merge (*) v1

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x y z) (Vec3 a b c) = Vec3 (y * c - z * b) (z * a - x * c) (x * b - y * a)

len2 :: (Foldable t, Num a, Applicative t) => t a -> a
len2 v = dot v v

len :: (Foldable t, Floating a, Applicative t) => t a -> a
len = sqrt . len2

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    pure a = Vec3 a a a
    (<*>) (Vec3 fx fy fz) (Vec3 x y z) = Vec3 (fx x) (fy y) (fz z)

instance Foldable Vec3 where
    foldMap f (Vec3 x y z) = f x <> f y <> f z
    foldr f start (Vec3 x y z) = f x $ f y $ f z start

instance Eq a => Eq (Vec3 a) where
    (==) = (and .) . merge (==)

instance Show a => Show (Vec3 a) where
    show (Vec3 x y z) = "V: " ++ show (x, y, z)

instance Num a => Num (Vec3 a) where
    (+) = merge (+)
    (*) = merge (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate

instance Fractional a => Fractional (Vec3 a) where
    fromRational = pure . fromRational
    (/) = merge (/)