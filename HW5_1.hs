module HW5_1
   ( rotateRight
   , sortList
   , multiPrimeFactor
   , sphenicList
   , calc
   ) where

import Data.List
import Data.Maybe


-- |Функция rotateRight принимает список произвольных данных и положительное число n
-- после чего производит циклическое вращение направо n раз. [a, b, c, d, e] -> [e, a, b, c, d]
--                      n
rotateRight :: [a] -> Int -> [a]
rotateRight xs 0     = xs
rotateRight [] _     = []
rotateRight xs n = rotateRight (last xs : init xs) (n - 1)


-- |Функция sortList принимает сортирует список по длине его подсписков.
sortList :: [[a]] -> [[a]]
sortList []   = []
sortList (l:ls) =
    let smallerThanHead = sortList [a | a <- ls, length a < length l]
        greaterThanHead = sortList [a | a <- ls, length a >= length l]
    in smallerThanHead ++ [l] ++ greaterThanHead

-- |Функция multiPrimeFactor по заданному числу стоит список из его простых делителей
-- и количества их вхождений. Список сортируется в порядке возрастания простых делителей.
--                          prime,count
multiPrimeFactor :: Int -> [(Int,Int)]
--multiPrimeFactor = error "multiPrimeFactor is not implemented"
multiPrimeFactor n = primeFactor n []

primeFactor :: Int -> [(Int,Int)] -> [(Int,Int)]
primeFactor 1 xs = xs
primeFactor n xs = primeFactor n' xs'
  where
    delimiter = fromJust $ find (\p -> n `mod` p == 0) (primes n)
    n'        = n `div` delimiter
    xs'       = insertDelimiter delimiter xs

insertDelimiter :: Int -> [(Int, Int)] -> [(Int, Int)]
insertDelimiter del pairs = case find (\pair -> fst pair == del) pairs of
    Nothing   -> (del, 1) : pairs
    Just pair -> let incrementedPair = (fst pair, 1 + snd pair)
                 in incrementedPair : filter (\p -> p /= pair) pairs




-- |Функция sphenicList возвращается список сфенических чисел в промежутке от А до В.
-- ru.wikipedia.org/wiki/Сфеническое_число
--              А      В
sphenicList :: Int -> Int -> [Int]
sphenicList a b = sort [x | x <- [a..b], x `elem` (sphenics b)]

sphenics :: Int -> [Int]
sphenics under  = [x * y * z |  x <- primes bound, y <- primes bound, z <- primes bound, x /= y && x /= z && y /= z]
    where bound = ceiling (fromIntegral under / 6)
sphenics2 bound = [x * y |  x <- primes bound, y <- primes bound, x /= y]

primes :: Int -> [Int]
primes bound = [x | x <- [2..bound], isPrime x]
isPrime p = p > 1 && all (\x -> mod p x /= 0) [2..floor(sqrt (fromIntegral p))]


intTest :: Int -> Int
intTest n = ceiling (fromIntegral n / 6)





-- |Функция  подсчитывает значение произвольного выражения записанного в символьной форме.
-- Пример, ["one","two","one","+","six","*","two","six"] -> (121 + 6) * 26 = 3302
-- Определенные операторы: +, -, *, /, % (деление по модулю) , ^ (степень числа)
-- Приоритета операций нет, потому все вычисления производятся слева направо последовательно.
calc :: [String] -> Int
calc [] = 0
calc xs = calc' (dropFirstNumber xs) (takeFirstNumber xs)

calc' [] res     = res
calc' (mark:xs) res = calc' afterNumber eval
    where
      next        = takeFirstNumber xs
      afterNumber = dropFirstNumber xs
      eval        = case mark of
            "+"              -> res + next
            "-"              -> res - next
            "*"              -> res * next
            "/"              -> res `div` next -- attention
            "%"              -> res `mod` next
            "^"              -> res ^ next



dropFirstNumber :: [String] -> [String]
dropFirstNumber xs = dropWhile isNumeric xs

takeFirstNumber :: [String] -> Int
takeFirstNumber xs = foldl (\acc s -> acc * 10 + numberfromString s) 0 (takeWhile isNumeric xs)

isNumeric s = isJust $ find (\p -> s == fst p) nums
numberfromString s = snd $ fromJust $ find (\p -> s == fst p) nums
nums = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9), ("zero", 0)]