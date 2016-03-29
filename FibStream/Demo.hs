{-
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]
-}

module Demo where

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation