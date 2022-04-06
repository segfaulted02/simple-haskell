module DailyOne where

quadratic :: Float -> Float -> Float -> Float -> Float
quadratic a b c x = a + b * x + c * x ** 2

scaleVector :: Float -> (Float, Float) -> (Float, Float)
scaleVector s (v1, v2) = (s * v1, s * v2)

tripleDistance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
tripleDistance (v1, v2, v3) (w1, w2, w3) = ((w1 - v1) ** 2 + (w2 - v2) ** 2 + (w3 - v3) ** 2) ** 1/2