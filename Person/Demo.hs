{-
Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1.
-}

module Demo where

data Person = Person { firstName :: String, lastName :: String, age :: Int }

infixl 1 &
x & f = f x

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 {lastName = (person1 & lastName)}