module HW2
       ( list2_1
       , gcd
       , coPrime
       , gcfList
       , perfect
       ) where

-- Запускать при помощи: haskellstack.org
-- stack setup - установка GHC нужной версии и т.д.
-- stack build - компиляция
-- stack test  - тесты

-- |Написать программу для нахождения минимального из
-- чисел, являющихся максимальными в каждой из строк заданной
-- прямоугольной матрицы.
list2_1 :: Integral a => [[a]] -> a
list2_1 n = minimum $ map maximum n

-- |Вычислить наибольший общий делитель(НОД) 2х натуральных чисел. Использовать алгоритм Евклида.
gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' 0 a = a
gcd' a b = gcd' (greater `mod` lesser) lesser
    where greater = if (a > b) then a else b
          lesser  = if (a > b) then b else a

-- |Проверить являются ли 2 натуральных числа взаимнопростыми.
-- 2 числа называются взаимнопростыми если их НОД равен 1
coPrime :: Integral a => a -> a -> Bool
coPrime a b = gcd' a b == 1

-- |Напишите функцию нахождения самого маленького числа, которое делится
-- на все числа от одного до N.
gcfList :: Integral a => a -> a
gcfList n = [x | x <- [1..], matches x n ]!!1
    where matches x n = all (\y -> x `mod` y == 0) [1..n]

-- |Написать программу для нахождения первых N совершенных чисел.
-- Совершенным числом называется натуральное число, равное
-- сумме всех своих делителей, включая единицу, но исключая само
-- это число. Так, например, число 28 – совершенное, поскольку
-- 28 = 1 + 2 + 4 + 7 + 14. 
perfect :: Integral a => a -> [a]
perfect a = [x | x <- [1..], isPerfect x]

isPerfect :: Integral a => a -> Bool
isPerfect a = sum delimiters == a
    where delimiters = [x | x <- [1..a `div` 2], a `mod` x == 0]