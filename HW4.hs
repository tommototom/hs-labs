module HW4
   ( insertValue
   , slice
   , collectNodes
   , isEulerPathExist
   , paths
   ) where

import Data.List
import Data.Maybe

lastIndex :: [a] -> Int
lastIndex xs = length xs - 1

-- |Функция insertValue должна должна вставлять значение в список на указанную
-- позицию.
insertValue :: a -> [a] -> Int -> [a]
--insertValue val xs 0 = length xs - 1
insertValue val xs pos = let l = length xs - 1 in
    case pos of
        x | x > l     -> error ("attempt to insert at " ++ show x ++ " position in list with size " ++ (show $ length xs))
        0             -> [val] ++ xs
        l             -> xs ++ [val]
        otherwise     -> let left  = fromJust $ slice xs 0 pos
                             right = fromJust $ slice xs (pos) (length xs - 1)
                         in left ++ [val] ++ right

-- |Функция slice должна возвращать подсписок в пределах от A до B
--      список     А     B
slice :: [a] -> Int -> Int -> Maybe [a]
slice xs a b = case () of
    () | a >= length xs -> Nothing
       | b >= length xs -> Nothing
       | a > b          -> Nothing
       | otherwise      -> Just (
            let applyLeft = snd (splitAt a xs)
                applyRight = fst (splitAt b applyLeft)
            in applyRight
       )

data Tree a = Node a [Tree a] deriving (Show)

-- |Написать функцию collectNodes, которая для каждого уровня дерева составит список
-- значений узлов и выведет общий список всех уровней.
--  Node 1 [Node 2 [Node 4 [], Node 3 []], Node 5 [Node 6 [Node 7 []]]]
collectNodes :: Tree a -> [[a]]
collectNodes (Node v ts) = [v] : collectNodes' ts []

collectNodes' :: [Tree a] -> [[a]] -> [[a]]
collectNodes' [] res    = res
collectNodes' level res = res ++ [levelValues level] ++ collectNodes' (nodesBelow level) res

levelValues :: [Tree a] -> [a]
levelValues ts = map (\(Node n _) -> n) ts

nodesBelow :: [Tree a] -> [Tree a]
nodesBelow xs = flatmap treesOf xs



treesOf :: Tree a -> [Tree a]
treesOf (Node _ xs) = xs

flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs

-- |Функция isEulerPathExist должна проверить содержит ли граф Эйлеров путь
type Graph = [(Int, [Int])]

isEulerPathExist :: Graph -> Bool
-- Эйлеров путь существует тогда и только тогда, когда количество вершин с нечётными степенями равно двум (или нулю, в случае существования эйлерова цикла).
isEulerPathExist g = length matchingEdges == 2
   where matchingEdges = filter (\edges -> odd $ length edges) (map snd g)





-- |Функция paths должна вернуть список путей между 2мя точками в графе.
-- Каждый путь представляется собой список с вершинами от точки А до B.
-- Граф задается как список ребер [(a,a)]. Например, [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
--               А   B     граф
paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths a b g = map (\path -> a : path) (pathsInternal a b g [a])
  where
    pathsInternal a b g visited
        | a == b    = [[]]
        | otherwise = [c:path | c <- neighbors a g, c `notElem` visited, path <- pathsInternal c b g (c:visited)]

-- возвращает всех соседей ноды "a" для ориентированного графа "g"
neighbors :: Eq a => a -> [(a,a)] -> [a]
neighbors a g = map snd filtered
  where
    filtered = filter (\edge -> fst edge == a) g