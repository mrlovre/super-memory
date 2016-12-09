module Utility where


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
f `uncurry3` (x, y, z) = f x y z
