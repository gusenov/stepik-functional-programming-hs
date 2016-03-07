module Integration where

-- https://stepic.org/lesson/%D0%9B%D0%BE%D0%BA%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5-%D1%81%D0%B2%D1%8F%D0%B7%D1%8B%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%B8-%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D0%B0-%D0%BE%D1%82%D1%81%D1%82%D1%83%D0%BF%D0%BE%D0%B2-8414/step/9?unit=1553

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b =
 let
  h = (b - a) / 100000
  aHalf = h / 2
  y x s | a == 0 && b == 0        = 0 
        | x == a                  = y (a + h) (s + f a)
        | a < b && x > a && x < b = middle
        | a > b && x < a && x > b = middle
        | a < b && x >= b         = end
        | a > b && x <= b         = end
   where
    middle = y (x + h) (s + 2 * f x)
    end = s + (f b)
 in aHalf * (y a 0)