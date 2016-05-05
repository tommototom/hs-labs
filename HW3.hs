module HW3
       ( root
       , sequence2_3_f
       , symmetric
       , listDigits
       , isHeap
       ) where

import Data.List
import Data.Char

-- |Функция root должна вычислить приближенное значение корня уравнения
-- tan x = 1 - x с точностью, заданной первым (и единственным) аргументом
-- функции
f :: Double -> Double
f x = tan x + x - 1

root :: Double -> Double
root eps = error "root is not implemented"

-- |Бесконечная упорядоченная последовательность целых чисел без повторений
-- составлена из всех квадратов, кубов и факториалов натуральных чисел
-- ([1, 2, 4, 8, 9, 16, 24, 25, 27, 36, 49, 64, ..]). Написать программу для
-- вычисления n-го члена этой последовательности.
sequence2_3_f :: Int -> Int
sequence2_3_f n = ((nub . sort) (merged))!!n
        where merged = mergeThree (take n squares) (take n cubes) (take n factorials)

squares =    [x * x | x <- [1..]]
cubes =      [x * x * x | x <- [1..]]
factorials = [x ^ x | x <- [1..]]

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

mergeThree xs ys zs = merge xs (merge ys zs)


-- |Написать функцию symmetric для проверки симметричности бинарного дерева.
-- Бинарное дерево является симметричным, если, проведя вертикальную линию
-- через корневой узел, правое поддерево будет являться зеркальным отражением
-- левого поддерева. Сравнивается лишь симметрия структуры дерева. Значение в
-- узле сравнивать не надо.
data Tree a = Empty | Node (Tree a) a (Tree a)

symmetric ::  Tree a -> Bool
symmetric Empty = True
symmetric (Node leftTree _ rightTree) = compareTrees leftTree rightTree

compareTrees :: Tree a -> Tree a -> Bool
compareTrees Empty Empty = True
compareTrees Empty (Node _ _ _) = False
compareTrees (Node _ _ _) Empty  = False
compareTrees (Node aLeft _ aRight) (Node bLeft _ bRight) = compareTrees aLeft bLeft && compareTrees aRight bRight

-- |Написать функцию listDigits, которая для заданного дерева из строк выдает
-- список всех строк, содержащих хотя бы одну цифру.

listDigits :: Tree String -> [String]
listDigits Empty = []
listDigits (Node leftTree str rightTree) = listDigits(leftTree) ++ listDigits(rightTree) ++
    if containsDigits(str) then [str] else []

containsDigits :: String -> Bool
containsDigits str = any isDigit str

-- |Функция isHeap проверяет является ли дерево пирамидой, то есть значение
-- в каждом из его узлов меньше значений, хранящихся в поддеревьях у этого узла.
data MultiTree a = Branch a [MultiTree a] deriving (Show)

isHeap :: Ord a => MultiTree a -> Bool
isHeap (Branch _ []) = True
isHeap (Branch x xs) = all (\(Branch y _) -> x < y) xs && all isHeap xs