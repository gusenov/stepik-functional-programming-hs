import Control.Monad (liftM, ap)

-- В следующей серии задач вам нужно будет разработать стандартный интерфейс для типа данных

data OddCont a = Singl a | BiCons a a (OddCont a)  deriving (Eq,Show)

{- Этот тип представляет собой контейнер-последовательность, который по построению может содержать только нечетное число элементов:

GHCi> let cont1 = Singl 42
GHCi> let cont3 = BiCons 1 2 cont1
GHCi> let cont5 = BiCons 3 4 cont3
GHCi> cont5
BiCons 3 4 (BiCons 1 2 (Singl 42))
GHCi> let contInf = BiCons 'A' 'B' contInf
GHCi> contInf
BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'B' (BiCons 'A' 'Interrupted.
GHCi>

Подразумевается, что реализацию интерфейса для типа OddCont вы будете осуществлять, исходя из «первопринципов», 
то есть пользоваться сведением к стандартным контейнерным типам запрещено.
-}

-- Реализуйте функции 
lenOC :: OddCont a -> Int 
-- и 
countConsOC :: OddCont a -> Int
{-
Первая должна возвращать число элементов в контейнере, а вторая — число конструкторов данных типа OddCon, использованных при построении контейнера.

GHCi> let testCont = BiCons 'a' 'b' (BiCons 'c' 'd' (Singl 'e'))
GHCi> lenOC testCont
5
GHCi> countConsOC testCont
3
-}

lenOC (Singl a) = 1
lenOC (BiCons a b c) = 2 + lenOC c

countConsOC (Singl a) = 1
countConsOC (BiCons a b c) = 1 + countConsOC c




-- Реализуйте функции 
headOC,lastOC :: OddCont a -> a
{-
Первая должна возвращать «самый левый» элемент в контейнере, а вторая — «самый правый».

GHCi> let testCont = BiCons 'a' 'b' (BiCons 'c' 'd' (Singl 'e'))
GHCi> headOC testCont
'a'
GHCi> lastOC testCont
'e'
-}
headOC (Singl a) = a
headOC (BiCons a b c) = a

lastOC (Singl a) = a
lastOC (BiCons a b c) = lastOC c




-- Реализуйте функции 
sumOC :: Num a => OddCont a -> a
findOC :: (a -> Bool) -> OddCont a -> Maybe a
{-
Первая должна суммировать все элементы в контейнере, а вторая — возвращать «самый левый» элемент контейнера, удовлетворяющий заданному предикату, если такой найдется.

GHCi> let testCont = BiCons 5 6 (BiCons 7 8 (Singl 9))
GHCi> sumOC testCont
35
GHCi> findOC (> 7) testCont
Just 8
GHCi> findOC (< 5) testCont
Nothing
-}

sumOC (Singl a) = a
sumOC (BiCons a b c) = a + b + (sumOC c)

findOC p (Singl a) = if p a then Just a else Nothing 
findOC p (BiCons a b c) | p a = Just a
                        | p b = Just b
                        | otherwise = findOC p c




-- Реализуйте функцию 
takeNCons :: Int -> OddCont a -> OddCont a
takeNCons n _ | n <= 0 = error "Only positive n allowed!"
{- которая принимает положительное число n и контейнер. Она должна возвращать ту часть элементов исходного контейнера, которая удерживается n левыми конструкторами, отбрасывая все, кроме первых 2n-1 элементов. Если контейнер «короче», чем n конструкторов, он не меняется.

GHCi> let testCont = BiCons 'a' 'b' (BiCons 'c' 'd' (Singl 'e'))
GHCi> takeNCons 1 testCont
Singl 'a'
GHCi> takeNCons 2 testCont
BiCons 'a' 'b' (Singl 'c')
GHCi> takeNCons 3 testCont
BiCons 'a' 'b' (BiCons 'c' 'd' (Singl 'e'))
GHCi> takeNCons 4 testCont
BiCons 'a' 'b' (BiCons 'c' 'd' (Singl 'e'))
-}

takeNCons 1 (Singl a) = Singl a
takeNCons 1 (BiCons a b c) = Singl a
takeNCons n (Singl a) | n >= 2 = Singl a
takeNCons n (BiCons a b c) = BiCons a b (takeNCons (n - 1) c)




{- Сделайте тип данных OddCont представителем класса типов Functor.

GHCi> let testCont = BiCons 'a' 'b' (BiCons 'c' 'd' (Singl 'e'))
GHCi> fmap succ testCont
BiCons 'b' 'c' (BiCons 'd' 'e' (Singl 'f'))
-}
instance Functor OddCont where
  fmap foo (Singl a) = Singl (foo a)
  fmap foo (BiCons a b c) = BiCons (foo a) (foo b) (fmap foo c)




-- Реализуйте функцию 
foldrOC :: (a -> b -> b) -> b -> OddCont a -> b

{- Она должна обеспечивать для типа OddCont поведение, эквивалентное поведению функции foldr для списков.

GHCi> let testCont = BiCons 5 6 (BiCons 7 8 (Singl 9))
GHCi> foldrOC (+) 0 testCont
35
GHCi> foldrOC const undefined testCont
5
-}
foldrOC f ini (Singl a) = f a ini
foldrOC f ini (BiCons a b c) = f a (f b (foldrOC f ini c))




-- Реализуйте функцию 
concat3OC :: OddCont a -> OddCont a -> OddCont a -> OddCont a
{- конкатенирующую три контейнера в один:

GHCi> let tst1 = BiCons 'a' 'b' (Singl 'c')
GHCi> let tst2 = BiCons 'd' 'e' (BiCons 'f' 'g' (Singl 'h'))
GHCi> let tst3 = BiCons 'i' 'j' (Singl 'k')
GHCi> concat3OC tst1 tst2 tst3
BiCons 'a' 'b' (BiCons 'c' 'd' (BiCons 'e' 'f' (BiCons 'g' 'h' (BiCons 'i' 'j' (Singl 'k')))))
Обратите внимание, что соображения четности запрещают конкатенацию двух контейнеров OddCont.
-}

concat3OC t1 t2 t3 = lst2OddCont $ (oddCont2Lst t1) ++ (oddCont2Lst t2) ++ (oddCont2Lst t3)

oddCont2Lst (Singl a) = [a]
oddCont2Lst (BiCons a b c) = [a] ++ [b] ++ (oddCont2Lst c)

lst2OddCont :: [a] -> OddCont a
lst2OddCont [x] = Singl x
lst2OddCont (x:y:lst) = BiCons x y (lst2OddCont lst)




-- Реализуйте функцию 
concatOC :: OddCont (OddCont a) -> OddCont a
{- Она должна обеспечивать для типа OddCont поведение эквивалентное поведению функции concat для списков.

GHCi> concatOC $ Singl (Singl 42)
Singl 42
GHCi> let tst1 = BiCons 'a' 'b' (Singl 'c')
GHCi> let tst2 = BiCons 'd' 'e' (BiCons 'f' 'g' (Singl 'h'))
GHCi> let tst3 = BiCons 'i' 'j' (Singl 'k')
GHCi> concatOC $ BiCons tst1 tst2 (Singl tst3)
BiCons 'a' 'b' (BiCons 'c' 'd' (BiCons 'e' 'f' (BiCons 'g' 'h' (BiCons 'i' 'j' (Singl 'k')))))
-}

concatOC = lst2OddCont . oddCont2Lst' 

oddCont2Lst' (Singl a) = oddCont2Lst a
oddCont2Lst' (BiCons a b c) = (oddCont2Lst a) ++ (oddCont2Lst b) ++ (oddCont2Lst' c)




{- Сделайте тип данных OddCont, представителем класса типов Monad. Семантика должна быть подобной семантике представителя Monad для списков: монада OddCont должна иметь эффект вычисления с произвольным нечетным числом результатов.

GHCi> let tst1 = BiCons 10 20 (Singl 30)
GHCi> let tst2 = BiCons 1 2 (BiCons 3 4 (Singl 5))
GHCi> do {x <- tst1; y <- tst2; return (x + y)}
BiCons 11 12 (BiCons 13 14 (BiCons 15 21 (BiCons 22 23 (BiCons 24 25 (BiCons 31 32 (BiCons 33 34 (Singl 35)))))))
GHCi> do {x <- tst2; y <- tst1; return (x + y)}
BiCons 11 21 (BiCons 31 12 (BiCons 22 32 (BiCons 13 23 (BiCons 33 14 (BiCons 24 34 (BiCons 15 25 (Singl 35)))))))
Функцию fail можно не реализовывать, полагаясь на реализацию по умолчанию.
-}

instance Monad OddCont where
 return = Singl
 xs >>= k = concatOC (fmap k xs)

{-
Любой тип, реализующий Monad, должен также реализовать Applicative, а любой тип, реализующий Applicative, должен реализовать функтор.
Поскольку любая монада всегда является аппликативным функтором, а любой аалпикативный функтор — функтором, 
соответствущие инстансы можно получить универсальным способом, подходящим для любого типа:
-}
instance Applicative OddCont where
 pure  = return
 (<*>) = ap
