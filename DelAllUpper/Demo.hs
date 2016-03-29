{-
Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре. Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.

GHCi> delAllUpper "Abc IS not ABC"
"Abc not"

Постарайтесь реализовать эту функцию как цепочку композиций, аналогично revWords из предыдущего видео.
-}

module Demo where

import Data.Char

delAllUpper :: String -> String
delAllUpper [] = []
delAllUpper xs = if (length (foo xs)) > 0 then init . foo $ xs else foo $ xs 

foo = concatMap upperWord . words
upperWord w = if all isUpper w then "" else w ++ " "