module HW5_2
   ( bottom_up
   , internalPathLen
   , сonnectedСomponent
   , maxPath
   , bipartiteGraph
   ) where

import Data.List
import Data.Maybe

-- |Написать функцию bottom_up, которая построит список узлов дерева от листьев к корню.
data Tree a = Node a [Tree a] deriving (Show)

bottom_up  :: Tree a -> [a]
bottom_up node = reverse $ flatmap (\x -> x) (collectNodes node)

-- |Написать функцию internalPathLen, которая подсчитает сумму всех путей дерева от корня к листьям.
internalPathLen :: Tree a -> Int
internalPathLen (Node val []) = 1
internalPathLen (Node val xs) = sum $ map internalPathLen xs

-- |Написать функцию сonnectedСomponent, которая проверяет не содержит ли граф больше N компонент связанности.
type Graph = [(Int, [Int])]
--                              N
сonnectedСomponent :: Graph -> Int -> Bool
сonnectedСomponent graph n = connectedAmount(graph) > n

connectedAmount :: Graph -> Int
connectedAmount nodes = length $ nub $ map sort $ map (\node -> dfs nodes [] [fst node]) nodes

--dfs :: Graph -> [Int] -> [Int]
dfs graph visited [] = reverse visited
dfs graph visited (x:xs)
    | x `elem` visited = dfs graph visited xs
    | otherwise        = dfs graph (x:visited) (neighbors ++ xs)
    where neighbors = snd $ fromJust $ find (\it -> fst it == x) graph


-- |Написать функцию maxPath, которая выдает список вершин с наибольшей длиной маршрута между двумя заданными
-- вершинами. Если максимальных маршрутов несколько, то вывести все. Если маршрута между вершинами не существует,
-- то функция должна выдавать пустой список.
maxPath :: Graph -> Int -> Int -> [[Int]]
maxPath graph a b = case allPaths of
    [] -> []
    xs -> filter (\path -> length path == maxPathLen) allPaths
    where
      allPaths   = paths a b (edges graph)
      maxPathLen = length $ last $ sort allPaths


edges graph = let allEdges = flatmap (\(val, nodes) -> map (\e -> (val, e)) nodes) graph
              in  nub $ map (\(l,r) -> if l > r then (r, l) else (l, r)) allEdges

-- |Написать функцию bipartiteGraph, которая проверяет, является ли граф двудольным.
bipartiteGraph :: Graph -> Bool
bipartiteGraph = error "bipartiteGraph is not implemented"

--                       left     right               'to visit'   two halfs
bipartiteDfs :: Graph -> [Int] -> [Int] -> Bool -> [Int]-> Maybe ([Int], [Int])
bipartiteDfs graph leftVisited rightVisited fromLeft []     = Just (leftVisited, rightVisited)
bipartiteDfs graph leftVisited rightVisited fromLeft (x:xs)
    | consistencyBroken = Nothing
    | visited           = bipartiteDfs graph leftVisited     rightVisited     (not fromLeft) xs
    | fromLeft          = bipartiteDfs graph leftVisited     (x:rightVisited) (not fromLeft) (neighbors ++ xs)
    | otherwise         = bipartiteDfs graph (x:leftVisited) rightVisited     (not fromLeft) (neighbors ++ xs)
    where
      neighbors         = snd $ fromJust $ find (\it -> fst it == x) graph
      consistencyBroken = fromLeft && x `elem` rightVisited || (not fromLeft) && x `elem` leftVisited
      visited           = x `elem` leftVisited || x `elem` leftVisited



-- (import is hard)

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