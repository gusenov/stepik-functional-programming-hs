{-
Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.

GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]

Разрешается использовать только функции, доступные из библиотеки Prelude.
-}

module Demo where

groupElems :: Eq a => [a] -> [[a]]
groupElems x = bar (foo x)

foo :: Eq a => [a] -> [[a]]
foo [] = []
foo (x:xs) = [x]:(foo xs)

bar :: Eq a => [[a]] -> [[a]]
bar [] = []
bar [x] = [x]
bar ((x:xs):(y:ys):zs) = if (x == y) then bar ((y:x:xs):zs) else (x:xs):(bar ((y:ys):zs))
