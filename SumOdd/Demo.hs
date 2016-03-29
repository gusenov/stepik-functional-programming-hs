{-
Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения:

GHCi> sumOdd [2,5,30,37]
42
-}

module Demo where

foo x y = if (odd x) then x + y else y

sumOdd :: [Integer] -> Integer
sumOdd = foldr foo 0

-- (2 + (5 + (30 + (37 + 0))))