module Ray (Ray (..), mv) where
    
import Vec3 (Direction, Point)

data Ray = Ray
    { origin :: Point
    , direction :: Direction
    , refractiveIndex :: Float
    }

mv :: Ray -> Float -> Point
mv r d = origin r + pure d * direction r
