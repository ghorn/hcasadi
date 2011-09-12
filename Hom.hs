-- Hom.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Hom ( Ode
           , Cost
           , dA
           , dB
           , Quad(..)
           , evalQuad
           , checkQuad
           ) where

import Casadi
import Control.DeepSeq

type Ode a = a -> a -> a
type Cost a = a -> a -> a

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


evalQuad :: (Matrix a b c) => (Quad a) -> a -> a
evalQuad (Quad h g a x0) x = a + dx'*g + (0.5)*(dx'*h*dx)
   where
     dx = x - x0
     dx' = trans(dx)

checkQuad :: (Matrix a b c) => (Quad a) -> String
checkQuad (Quad h g a x0) = ret
  where
    n = rows x0
    
    h' = ((n,n) == size h)
    g' = ((n,1) == size g)
    a' = ((1,1) == size a)
    x0' = ((n,1) == size x0)
    
    ret
      | and [h', g', a', x0'] = "quad OK, bro"
      | otherwise             = error errMsg
    
    errMsg = "quad dimensions error\n" ++ 
             "size h:  " ++ show (size h)  ++ "\n" ++
             "size g:  " ++ show (size g)  ++ "\n" ++
             "size a:  " ++ show (size a)  ++ "\n" ++
             "size x0: " ++ show (size x0)
