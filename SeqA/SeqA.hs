module SeqA where

// https://stepic.org/lesson/%D0%9B%D0%BE%D0%BA%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5-%D1%81%D0%B2%D1%8F%D0%B7%D1%8B%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%B8-%D0%BF%D1%80%D0%B0%D0%B2%D0%B8%D0%BB%D0%B0-%D0%BE%D1%82%D1%81%D1%82%D1%83%D0%BF%D0%BE%D0%B2-8414/step/6?unit=1553

seqA :: Integer -> Integer
seqA n  | n == 0 = 1
        | n == 1 = 2
        | n == 2 = 3
        | otherwise = helper (n - 2) 1 2 3
    where
        helper n a b c | n == 0 = c
                       | otherwise = helper (n - 1) b c (c + b - 2 * a)