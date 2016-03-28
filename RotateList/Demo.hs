{-
Реализуйте функцию, задающую циклическое вращение списка.

rotate :: Int -> [a] -> [a]
rotate n xs = undefined

При положительном значении целочисленного аргумента вращение должно осуществляться влево, при отрицательном - вправо.

GHCi> rotate 2 "abcdefghik"
"cdefghikab"
GHCi> rotate (-2) "abcdefghik" 
"ikabcdefgh"

Не забывайте обеспечить работоспособность вашей реализации на бесконечных списках (для сценариев, когда это имеет смысл) и разумную эффективность при большом числе вращений небольшого списка:

GHCi> :set +s
GHCi> rotate 1234567890 [1..10]
[1,2,3,4,5,6,7,8,9,10]
(0.00 secs, 0 bytes)
-}

module Demo where

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = case drop n xs of 
    [] -> helper (mod n $ length xs) xs
    _  -> helper n xs
    
helper _ [] = []
helper 0 xs = xs
helper n xs | n > 0 = drop n xs ++ take n xs
            | n < 0 = let shift = length xs - (-n) in helper shift xs

{-
https://wiki.haskell.org/99_questions/Solutions/19
http://langref.org/fsharp+clojure+scala+haskell/lists/modification/rotate
http://www.cs.unc.edu/~bcw/comp524-sp14/examples/haskell/soln.hs
-}