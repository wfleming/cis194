{-# OPTIONS_GHC -Wall #-}
module Golf (
  skips,
  localMaxima
) where

skips :: [a] -> [[a]]
skips as = skips' 1
  where
    skips' n
      | n > length as = []
      | otherwise  = (skipN as n) : (skips' (n + 1))

withGroups xs n = zip xs (cycle (take n (iterate (+1) 1)))
skipN xs n = map fst (filter (\(_, idx) -> idx == n) (withGroups xs n))

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map takeMax (filter isMax (triples xs))
  where
    takeMax (_, x, _) = x
    isMax (x1, x2, x3) = x1 < x2 && x2 > x3

triples :: [a] -> [(a, a, a)]
triples (x0 : x1 : x2 : rest) =  (x0, x1, x2) : (triples (x1 : x2 : rest))
triples _ =  []
