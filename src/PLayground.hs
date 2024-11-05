module Playground where

collatz :: Integer -> [Integer]
collatz n
  | n == 1 = [1]
  | even n = n : collatz (div n 2)
  | odd n = n : collatz (3 * n + 1)
  | otherwise = error "collatz: invalid argument"
