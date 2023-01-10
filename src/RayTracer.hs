module RayTracer where
import Primitives (Primitive)
import Color (Color)
import Ray (Ray)
import Scene (Scene)


evaluate :: Scene Primitive -> Ray -> Color
evaluate = undefined