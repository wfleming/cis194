module Hw4 where

--- EXERCISE 1 ----

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate iteratee
  where
    iteratee x = if even x then x `div` 2 else 3 * x + 1

--- EXERCISE 2 ----

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl insert Leaf

insert :: Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf
insert (Node _ l v r) x =
  if height l > height r
    then Node nextHeight l v (insert r x)
    else Node nextHeight (insert l x) v r
  where
    nextHeight = 1 + maximum [height l, height r]

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

--- EXERCISE 3 ----

xor :: [Bool] -> Bool
xor = foldl step False
  where
    step rv True = not rv
    step rv False = rv

map' :: (a -> b) -> [a] -> [b]
map' fn = foldr (\x rv -> fn x : rv) []

--- EXERCISE 4 ----
-- TODO: sieve of sundaram
