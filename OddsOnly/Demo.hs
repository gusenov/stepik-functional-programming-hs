{-
Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.

GHCi> oddsOnly [2,5,7,10,11,12]
[5,7,11]

Для анализа четности можно использовать функции odd и even стандартной библиотеки.
-}

module Demo where

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [x] = if odd x then [x] else []
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs
oddsOnly _ = []