module SumAndCount where

// https://stepic.org/lesson/%D0%9B%D0%BE%D0%BA%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5-%D1%81%D0%B2%D1%8F%D0%B7%D1%8B%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%B8-%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D0%B0-%D0%BE%D1%82%D1%81%D1%82%D1%83%D0%BF%D0%BE%D0%B2-8414/step/8?unit=1553

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
              | otherwise = let 
                    helper 0 s c = (s,c)
                    helper n s c = helper (quot n 10) (s + rem n 10) (c + 1)
			    in helper (abs x) 0 0