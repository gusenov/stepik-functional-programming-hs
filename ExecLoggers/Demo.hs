module Demo where

{- Введём следующий тип: -}
data Log a = Log [String] a deriving Show
{- Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger -}
toLogger :: (a -> b) -> String -> (a -> Log b)
{- которая превращает обычную функцию, в функцию с логированием: -}

{-
GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6

Далее, определите функцию execLoggers -}
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c

{-
Которая принимает некоторый элемент и две функции с логированием. 
execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, 
которые были выданы при применении каждой из функций:

GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
-}

toLogger f msg = \x -> Log [msg] (f x)
execLoggers x f g = Log ( (getMsg (f x)) ++ (getMsg (g (getValue (f x)))) ) (getValue (g (getValue (f x))))

getValue :: Log b -> b
getValue (Log _ a) = a

getMsg :: Log b -> [String]
getMsg (Log msg _) = msg

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"