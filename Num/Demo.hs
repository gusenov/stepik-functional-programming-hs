module Demo where

class Num a where
 (+), (-), (*) :: a -> a -> a
 negate :: a -> a -- унарный префиксный минус
 abs :: a -> a -- модуль числа
 signum :: a -> a -- знак числа
 fromInteger :: Integer -> a -- переход от значений типа Integer к любому числовому типу
 -- реализация по умолчанию:
 x - y = x + negate y
 negate x = 0 - x

{-
LAW abs x * signum x == x
-}