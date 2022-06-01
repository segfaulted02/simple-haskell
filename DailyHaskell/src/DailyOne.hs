module DailyOne where

--function that calculates a quadratic function
--accepts four float values then returns a float answer
quadratic :: Float -> Float -> Float -> Float -> Float
quadratic a b c x = a + b * x + c * x ** 2

--function that scales a vector according to a scalar value
--accepts a float scalar and a 2D vector then returns a 2D vector
scaleVector :: Float -> (Float, Float) -> (Float, Float)
scaleVector s (v1, v2) = (s * v1, s * v2)

--function that calculates the distance between two vectors
--accepts two 3D vectors then returns a float value
tripleDistance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
tripleDistance (v1, v2, v3) (w1, w2, w3) = ((w1 - v1) ** 2 + (w2 - v2) ** 2 + (w3 - v3) ** 2) ** 1/2