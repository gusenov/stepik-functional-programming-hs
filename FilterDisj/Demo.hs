{-
–еализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую список элементов, удовлетвор€ющих хот€ бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]
-}
module Demo where

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 [] = []
filterDisj p1 p2 (x:xs)
 | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
 | otherwise        = filterDisj p1 p2 xs