module Demo where

class Enum a where
 -- функции движения по перечислительному типу
 succ, pred :: a -> a
 
 -- всем перечислениям может быть сопоставлен некоторый номер
 toEnum :: Int -> a
 fromEnum :: a -> Int




-- Класс типов Bounded служит для того чтобы указывать верхнюю и нижнюю границы.
class Bounded a where
 minBound, maxBound :: a
