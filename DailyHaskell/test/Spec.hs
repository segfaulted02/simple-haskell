-- file: Spec.hs
module Main where

main :: IO ()
main = putStrLn "Testing"

{-# OPTIONS_GHC -F -pgmF hspec-discover #-}