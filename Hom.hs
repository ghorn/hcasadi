-- Hom.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Hom (q,State(..), Action(..), Ode(..), eulerStep, rk4Step, dA, dB, qx, qu, qxx, quu, qxu, Quad(..), evalQuad) where

import Numeric.AD
import Data.Traversable
import Numeric.LinearAlgebra


type State a = [a]
type Action a = [a]
type Ode a = [a] -> Action a -> [a]

---------------- general stuff -----------------
eulerStep :: (Floating a) => ([a] -> [a] -> [a]) -> [a] -> [a] -> a -> [a]
eulerStep dxdt x u dt = map (*dt) (dxdt x u)

rk4Step :: (Floating a) => ([a] -> [a] -> [a]) -> [a] -> [a] -> a -> [a]
rk4Step dxdt x u dt = map (/6) $ addLists [k1, twok2, twok3, k4]
  where
    addLists (a:as) = foldl (\acc y -> zipWith (+) acc y) a as
    
    k1 = map (*dt) $ dxdt x u
    k2 = map (*dt) $ addLists [x, map (*0.5) k1]
    k3 = map (*dt) $ addLists [x, map (*0.5) k2]
    k4 = map (*dt) $ addLists [x, k3]
    
    twok2 = map (*2) k2
    twok3 = map (*2) k3

-- linearize dynamics f ~= f0 + dA*(x-x0) + dB*(u - u0)
dA :: Floating a => (forall s a. (Floating a, Mode s) =>
                     [AD s a] -> [AD s a] -> [AD s a]) -> ([a] -> [a] -> [[a]])
dA f x u = jacobian g x
  where
    g x' = f x' (map lift u)


dB :: Floating a => (forall s a. (Floating a, Mode s) =>
                     [AD s a] -> [AD s a] -> [AD s a]) -> ([a] -> [a] -> [[a]])
dB f x u = jacobian g u
  where
    g u' = f (map lift x) u'


---- quadratic expansion of cost(x,u)
qx :: Floating a => (forall s a. (Floating a, Mode s) =>
                     [AD s a] -> [AD s a] -> AD s a) -> ([a] -> [a] -> [a])
qu :: Floating a => (forall s a. (Floating a, Mode s) =>
                     [AD s a] -> [AD s a] -> AD s a) -> ([a] -> [a] -> [a])
qxx :: Floating a => (forall s a. (Floating a, Mode s) =>
                      [AD s a] -> [AD s a] -> AD s a) -> ([a] -> [a] -> [[a]])
quu :: Floating a => (forall s a. (Floating a, Mode s) =>
                      [AD s a] -> [AD s a] -> AD s a) -> ([a] -> [a] -> [[a]])
qxu :: Floating a => (forall s a. (Floating a, Mode s) =>
                      [AD s a] -> [AD s a] -> AD s a) -> ([a] -> [a] -> [[a]])


qx cost x u = grad g x
  where
    g x' = cost x' (map lift u)

qu cost x u = grad g u
  where
    g u' = cost (map lift x) u'

qxx cost x u = hessian g x
  where
    g x' = cost x' (map lift u)

quu cost x u = hessian g u
  where
    g u' = cost (map lift x) u'

qxu cost x u = jacobian g u
  where
    g u' = (qx cost) (map lift x) u'


qx' :: forall a. Floating a =>
       (forall s a. (Floating a, Mode s) => ([AD s a] -> [AD s a] -> AD s a))
       -> (forall s a. (Floating a, Mode s) => ([AD s a] -> [AD s a] -> [AD s a]))
       -> [a]
       -> [a]
       -> Quad a
       -> [a]

qx' cost dode x u (Quad vxx vx v0 x0) = grad g x
  where
    g x' = q' x' (map lift u) (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0
    
    q' :: forall s. Mode s => [AD s a] -> [AD s a] -> Quad (AD s a) -> AD s a
    q' = q cost dode


q :: Floating a => ([a] -> [a] -> a) -> ([a] -> [a] -> [a]) -> ([a] -> [a] -> Quad a -> a)
q cost dode = \x u v -> (cost x u) + (nextValue x u v)
  where
    nextValue x u (Quad vxx vx v0 x0) = evalQuad (Quad vxx vx v0 x0) (dode x u)


data Quad a = Quad [[a]] [a] a [a] deriving (Show)

evalQuad :: Floating a => Quad a -> [a] -> a
evalQuad (Quad h g a x0) x = constTerm + linearTerm + quadraticTerm
   where
     constTerm = a
     linearTerm = dotLists g x' -- put length check here
     quadraticTerm = (*0.5) $ dotLists x' $ map (\y -> dotLists y x') h -- put length check here
 
     dotLists :: Floating a => [a] -> [a] -> a
     dotLists x0 x1 = sum (zipWith (*) x0 x1)
     
     x' = zipWith (-) x x0
