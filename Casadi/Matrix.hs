-- Matrix.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Casadi.Matrix( Matrix(..)
                    , Boundable(..)
                    ) where

import Casadi.SXFunctionRaw

import Control.DeepSeq
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

class (NFData a, NFData b, Num a, Fractional a, Num b, Fractional b, Floating b, Boundable a) => Matrix a b c | a -> b c where
  trans :: a -> a
  size :: a -> (Int, Int)
  rows :: a -> Int
  cols :: a -> Int
  toList :: a -> [b]
  toLists :: a -> [[b]]
  fromList :: [b] -> a
  fromLists :: [[b]] -> a
  vertcat :: [a] -> a
  inv :: a -> a
  toSingleton :: a -> b
  scale :: b -> a -> a
  zeros :: (Int,Int) -> a

  newZeros :: (Int, Int) -> IO a
  getForeignPtr :: a -> ForeignPtr c
  c_sxFunctionEvaluate :: a -> CInt -> Ptr (Ptr c) -> CInt -> Ptr (Ptr c) -> Ptr SXFunctionRaw -> IO ()

  toSingleton = head . toList

class Boundable a where
  bound :: a -> (a, a) -> a
