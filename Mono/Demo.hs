module Demo where

{- Мономорфная функция: -}
mono :: Char -> Char
mono x = x

{- Частичное ограничение полиморфизма функции -}
{- Функция мономорфна 1 аргументу и полиморфна по 2 -}
semiMono :: Char -> a -> Char
semiMono x y = x
