{-# OPTIONS_GHC -Who-incomplete-patterns #-}
module DailyNine where

--function that checks if a Functor follows the First Law
--accepts a function then returns a boolean
firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
firstFunctorLaw a
    | fmap (id) a == id a = True
    | otherwise = False

--function that checks if a Functor follows the Second Law
--accepts two function and composes them then returns a boolean
secondFunctorLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
secondFunctorLaw a b c
    | fmap (a . b) c == fmap a (fmap b c) = True
    | otherwise = False

--function to properly test following functor laws
--accepts a Maybe Int type then return an Int
addMaybe :: Maybe Integer -> Maybe Integer
addMaybe a = a

--function to properly test following functor laws
--accepts a Maybe Int type then return an Int
subtractMaybe :: Maybe Integer -> Maybe Integer
subtractMaybe a = a