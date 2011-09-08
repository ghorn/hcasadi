-- Matrix.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Casadi.Matrix( Matrix(..) ) where

class (Num a, Fractional a, Num b, Fractional b, Floating b) => Matrix a b | a -> b where
  trans :: a -> a
  size :: a -> (Int, Int)
  rows :: a -> Int
  cols :: a -> Int
  toList :: a -> [b]
  toLists :: a -> [[b]]
  fromList :: [b] -> a
  fromLists :: [[b]] -> a
  concatMat :: [a] -> a
  inv :: a -> a
  toSingleton :: a -> b
  scale :: b -> a -> a
  zeros :: (Int,Int) -> a

  toSingleton = head . toList
