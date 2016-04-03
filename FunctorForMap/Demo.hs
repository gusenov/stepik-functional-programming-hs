module Demo where

{-
Определите представителя класса Functor для типов данных Entry и Map. Тип Map представляет словарь, ключами которого являются пары:
-}
data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show
{-
В результате должно обеспечиваться следующее поведение: fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.

GHCi> fmap (map toUpper) $ Map []
Map []

GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]
-}

{-
class Functor f where -- класс типа Functor параметризован переменной f
 fmap :: (a -> b) -> f a -> f b -- f используется не как тип, а как некоторая функция над типом, поэтому у этой переменной должен быть стрелочный kind

Функция fmap принимает:
 1) некоторую произвольную функцию;
 2) некоторый контейнерный тип (вместо f подставляется [] или Maybe) с типом элементов a.
Возвращает она тот же самый контейнерный тип, но с типом элементов b.
К каждому элементу контейнера применяется функция f и дальше упаковывается в тот же самый контейнер.
 -}

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1,k2) (f v)
-- (a -> b) -> f a -> f b
-- (a -> b) -> Entry k1 k2 a -> Entry k1 k2 b

instance Functor (Map k1 k2) where
    fmap f (Map []) = Map []
    fmap f (Map (x:xs)) = (\x (Map xs) -> Map (x:xs)) (fmap f x) (fmap f (Map xs))
-- (a -> b) -> f a -> f b
-- (a -> b) -> Map k1 k2 a -> Map k1 k2 b
