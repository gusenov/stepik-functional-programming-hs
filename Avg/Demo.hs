module Demo where

import Data.Function

{- Некаррированная функция -}
avg :: (Double,Double) -> Double
avg p = (fst p + snd p) / 2
