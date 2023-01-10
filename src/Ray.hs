module Ray (Ray (..), mv) where

import Vec3 (Direction, Point)

data Ray where
    Ray ::
        { origin :: Point
        , direction :: Direction
        , refractiveIndex :: Float
        } ->
        Ray

mv :: Ray -> Float -> Point
mv r d = origin r + pure d * direction r