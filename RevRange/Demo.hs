{-

Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = undefined

  GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"

-}

module Demo where

import Data.List
  
revRange :: (Char,Char) -> [Char]
revRange (a,b) = if (a <= b) then unfoldr g b else ""
  where g = (\x -> if x==(pred a) then Nothing else Just (x,pred x))