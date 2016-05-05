module HW1
       ( hw1_1
       , hw1_2
       , fact2
       , isPrime
       , primeSum
       ) where

-- Запускать при помощи: haskellstack.org
-- stack setup - установка GHC нужной версии и т.д.
-- stack build - компиляция
-- stack test  - тесты

-- |Вычислить сумму двух аргументов
hw1_1 :: Integer -> Integer -> Integer
hw1_1 a b = a + b

-- |Вычислить сумму N членов ряда
--
--
--  N
-- ---
-- \    1
--  >  ---
-- /   k^k
-- ---
-- k=1
--
-- Использовать fromIntegral для перевода из Integer в Double
hw1_2 :: Integer -> Double
hw1_2 n = sum $ map (\k -> 1.0 / fromIntegral (k ^ k)) [1,2..n]

-- |Вычислить двойной факториал
-- n!! = 1*3*5*...*n, если n - нечетное
-- n!! = 2*4*6*...*n, если n - четное
fact2 :: Integer -> Integer
fact2 n = product (if odd n then odds n else evens n)

odds n = [x | x <- [1,3..n]]
evens n = [x | x <- [2,4..n]]

-- |Проверить заданное число на простоту
-- Использовать div для целочисленного деления
-- или mod для остатка от деления
isPrime :: Integer -> Bool
isPrime p = p > 1 &&
            all (\x -> mod p x /= 0) [2..floor(sqrt (fromIntegral p))]

-- |Найти сумму всех простых чисел в диапазоне [a;b]
primeSum :: Integer -> Integer -> Integer
primeSum a b = sum (filter isPrime [a..b])

