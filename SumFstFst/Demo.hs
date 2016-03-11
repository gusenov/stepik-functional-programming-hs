module Demo where

import Data.Function

sumFstFst = (+) `on` helper where helper pp = fst $ fst pp

sumFstFst' = (+) `on` (\pp -> fst $ fst pp)

p1 = ((1,2),(3,4))
p2 = ((3,4),(5,6))
