{-# OPTIONS_GHC -Wall #-}
module Golf (
  skips,
  localMaxima,
  histogram
) where

import Data.List

skips :: [a] -> [[a]]
skips as = skips' 1
  where
    skips' n
      | n > length as = []
      | otherwise  = (skipN as n) : (skips' (n + 1))
    withGroups xs n = zip xs (cycle (take n (iterate (+1) 1)))
    skipN xs n = map fst $ filter (\(_, idx) -> idx == n) (withGroups xs n)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map takeMax (filter isMax (triples xs))
  where
    takeMax (_, x, _) = x
    isMax (x1, x2, x3) = x1 < x2 && x2 > x3

triples :: [a] -> [(a, a, a)]
triples (x0 : x1 : x2 : rest) =  (x0, x1, x2) : (triples (x1 : x2 : rest))
triples _ =  []

histogram :: [Integer] -> String
histogram xs = unlines $ transpose hlines
  where
    hcounts = counts xs
    maxc = maximum $ map snd hcounts
    hlines = map hline hcounts
    hline (n, c) = (replicate (maxc - c) ' ') ++ (replicate c '*') ++ "=" ++ (show n)

counts :: [Integer] -> [(Integer, Int)]
counts xs = sortBy (\(n0,_) (n1,_) -> compare n0 n1) $ unionBy (\(n0, _) (n1, _) -> n0 == n1) cs emptyCounts
  where
    cs = map (\xs' -> (head xs', length xs')) (group $ sort xs)

emptyCounts :: [(Integer, Int)]
emptyCounts = zip [0..9] (repeat 0)

