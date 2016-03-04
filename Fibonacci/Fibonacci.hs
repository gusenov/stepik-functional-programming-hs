module Fibonacci where


fibonacci :: Integer -> Integer
fibonacci n  | n == 0 = 0
             | n == 1 = 1
             | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
             | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
             | otherwise = undefined


-- https://wiki.haskell.org/The_Fibonacci_sequence#Tail_recursive			 
fibonacci' :: Integer -> Integer
fibonacci' n = helper n 0 1
helper n a b | n == 0 = a
             | n > 0  = helper (n - 1) b (a + b)
             | n < 0  = helper (n + 1) b (a - b)
             | otherwise = undefined
