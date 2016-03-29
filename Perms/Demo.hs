{-
Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.
-}

module Demo where

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [y | p <- perms xs, y <- interleave p]
  where
    interleave []     = [[x]]
    interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)

-- http://stackoverflow.com/a/24564307/2289640