{-
Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.

GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","")

В решении вам поможет функция isDigit из модуля Data.Char.
-}
module Demo where

import Data.Char

readDigits :: String -> (String, String)
readDigits s = span isDigit s