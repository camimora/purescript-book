module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

example :: Int -> Int -> Int -> Int
example x y z = foo + bar
  where
    foo = x * y
    bar = y * z

main :: Effect Unit
main = do
  log $ show $ example 10 2 3
