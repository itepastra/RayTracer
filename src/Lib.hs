module Lib (
    someFunc,
    module RayTracer,
) where

import RayTracer

someFunc :: IO ()
someFunc = putStrLn "someFunc"
