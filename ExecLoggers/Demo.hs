module Demo where

import Control.Monad (liftM, ap)

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



{-
Функции с логированием из предыдущего задания возвращают в качестве результата значение с некоторой дополнительной информацией в виде списка сообщений. 
Этот список является контекстом. Реализуйте функцию returnLog
returnLog :: a -> Log a
которая является аналогом функции return для контекста Log. 
Данная функция должна возвращать переданное ей значение с пустым контекстом.
-}
returnLog :: a -> Log a
returnLog a = Log [] a




{-
Реализуйте фукцию bindLog

bindLog :: Log a -> (a -> Log b) -> Log b
которая работает подобно оператору >>= для контекста Log.

GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8
-}
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log s a) l = (\(Log (ms) r) -> Log (s ++ ms) r) (l a)


{-
Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad:-}
instance Monad Log where
    return = returnLog
    (>>=) = bindLog
-- Используя return и >>=, определите функцию execLoggersList

execLoggersList :: a -> [a -> Log a] -> Log a
{- которая принимает некоторый элемент, список функций с логированием 
и возвращает результат последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений, 
которые возвращались данными функциями:

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
-}

execLoggersList x (f:[]) = f x
execLoggersList x (f:fs) = (\(Log (msg:_) _) (Log ms a) -> Log (msg:ms) a) (f x) (execLoggersList (getValue (f x)) fs)

{-
Любой тип, реализующий Monad, должен также реализовать Applicative, а любой тип, реализующий Applicative, должен реализовать функтор.
Поскольку любая монада всегда является аппликативным функтором, а любой аалпикативный функтор — функтором, 
соответствущие инстансы можно получить универсальным способом, подходящим для любого типа:
-}
instance Functor Log where
    fmap = liftM

instance Applicative Log where
    pure  = return
    (<*>) = ap
