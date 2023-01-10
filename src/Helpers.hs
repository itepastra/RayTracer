module Helpers (merge) where

merge :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
merge f v1 v2 = fmap f v1 <*> v2