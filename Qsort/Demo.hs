{-
Напишите реализацию функции qsort. Функция qsort должная принимать на вход список элементов и сортировать его в порядке возрастания с помощью сортировки Хоара: для какого-то элемента x изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x, и потом запускаться рекурсивно на обеих частях.

GHCi> qsort [1,3,2,5]
[1,2,3,5]
Разрешается использовать только функции, доступные из библиотеки Prelude.
-}

module Demo where

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = (qsort lesser) ++ [p] ++ (qsort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- https://wiki.haskell.org/Introduction#Quicksort_in_Haskell