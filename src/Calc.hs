{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser(parseExp)
import StackVM

------- EXERCISES 1-2
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add lhs rhs) = (eval lhs) + (eval rhs)
eval (Mul lhs rhs) = (eval lhs) * (eval rhs)

evalM :: Maybe ExprT -> Maybe Integer
evalM Nothing = Nothing
evalM (Just e) = Just $ eval e

evalStr :: String -> Maybe Integer
evalStr = fmap evalM $ parseExp Lit Add Mul

------- EXERCISES 3-4
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Num Mod7 where
  (Mod7 lhs) + (Mod7 rhs) = Mod7 $ mod (lhs + rhs) 7
  (Mod7 lhs) - (Mod7 rhs) = Mod7 $ mod (lhs - rhs) 7
  (Mod7 lhs) * (Mod7 rhs) = Mod7 $ mod (lhs * rhs) 7
  abs x = x -- should always be 0..6
  signum (Mod7 x) = Mod7 $ if 0 == x then x else 1
  fromInteger x = Mod7 (mod x 7)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add = (+)
  mul = (*)

------- EXERCISE 5
instance Expr Program where
  lit = undefined
  add = undefined
  mul = undefined

compile :: String -> Maybe Program
compile = undefined
{-compile = parseExp lit add mul-}
