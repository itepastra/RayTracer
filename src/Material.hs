module Material (Material (..), whiteMaterial) where

import Color (Color)
import Vec3 (Vec3 (..))

data Material where
    Opague :: {color :: Color} -> Material
    Glassy :: {ior :: Float} -> Material

whiteMaterial :: Material
whiteMaterial = Opague (Vec3 1 1 1)

instance Eq Material where
    (==) (Opague c1) (Opague c2) = c1 == c2
    (==) (Glassy r1) (Glassy r2) = r1 == r2
    (==) _ _ = False
