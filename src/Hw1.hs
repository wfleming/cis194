module Hw1 (
  lastDigit, dropLastDigit, toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate, hanoi
) where

-------------------
-- Exercises 1-4 --
-------------------

lastDigit :: Integer -> Integer
lastDigit = last . toDigits

dropLastDigit :: Integer -> Integer
dropLastDigit = listToNum 1 . reverse . init . toDigits
  where
    listToNum :: Integer -> [Integer] -> Integer
    listToNum base (h : t) = (base * h) + listToNum (base * 10) t
    listToNum _ _ = 0

toDigits :: Integer -> [Integer]
toDigits num = toDigitsAccum num []
  where
    toDigitsAccum n ds = case n > 0 of
      True -> toDigitsAccum n' ds'
        where
          ds' = (rem n 10) : ds
          n' = div n 10
      False -> ds

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubleEveryOther' []
  where
    doubleEveryOther' :: [Integer] -> [Integer] -> [Integer]
    doubleEveryOther' xs (h : t)  = case rem (length xs) 2 of
      0 -> doubleEveryOther' (xs ++ [h]) t
      _ -> doubleEveryOther' (xs ++ [2 * h]) t
    doubleEveryOther' xs [] = xs

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate :: Integer -> Bool
validate n = 0 == (rem checksum 10)
  where
    checksum = sumDigits $ concatMap toDigits (doubleEveryOther . toDigitsRev $ n)

----------------
-- Exercise 5 --
----------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src dst _ = [(src, dst)]
hanoi n src dst by = (hanoi (n - 1) src by dst) ++ [(src, dst)] ++ (hanoi (n - 1) by dst src)

-- list of pegs: [src, dst, *others]
hanoi' :: Integer -> [Peg] -> [Move]
hanoi' 1 src dst _ = [(src, dst)]
hanoi' n src dst others = () ++ [(src, dst)] ++ ()
