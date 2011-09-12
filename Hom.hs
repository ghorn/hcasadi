-- Hom.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Hom (Ode, Cost,
            dA, dB,
            Quad(..), evalQuad) where

import Casadi
import Control.DeepSeq

type Ode a = a -> a -> a
type Cost a b = a -> a -> b

---------- linearize dynamics f ~= f0 + dA*(x-x0) + dB*(u - u0) ---------
dA :: Ode SXMatrix -> SXMatrix -> SXMatrix -> SXMatrix
dA f x u = jacobian (f x u) x

dB :: Ode SXMatrix -> SXMatrix -> SXMatrix -> SXMatrix
dB f x u = jacobian (f x u) u

------------- quadratic data type -----------
--                   hess    grad   const    x0
data Quad a = Quad a a a a deriving (Show)

instance NFData (Quad a) where
  rnf (Quad h g a x0) = h `seq` g `seq` a `seq` x0 `seq` ()


evalQuad :: (Matrix a b c) => (Quad a) -> a -> b
evalQuad (Quad h g a x0) x = toSingleton $ a + dx'*g + (0.5)*(dx'*h*dx)
   where
     dx = x - x0
     dx' = trans(dx)
