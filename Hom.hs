{-# LANGUAGE RankNTypes #-}

module Hom (eulerStep, rk4Step, dA, dB, qx, qu, qxx, quu, qxu, Quad(..), evalQuad) where

import Numeric.AD
import Data.Traversable
import Numeric.LinearAlgebra

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



data Quad = Quad [[Double]] [Double] Double

evalQuad :: Quad -> [Double] -> Double
evalQuad (Quad h g a) x = constTerm + linearTerm + quadraticTerm
   where
     constTerm = a
     linearTerm = dotLists g x -- put length check here
     quadraticTerm = (*0.5) $ dotLists x $ map (\y -> dotLists y x) h -- put length check here
    
     dotLists :: [Double] -> [Double] -> Double
     dotLists x0 x1 = sum (zipWith (*) x0 x1)


-------------- spring specifically --------------
springDxdt :: (Floating a) => [a] -> [a] -> [a]
springDxdt state action = [v, (-k*x - b*v + u)/mass] where
  x    = state  !! 0
  v    = state  !! 1
  u    = action !! 0
  k    = 10
  b    = 0.1
  mass = 1

springEuler :: (Floating a) => [a] -> [a] -> a -> [a]
springEuler x u dt = eulerStep springDxdt x u dt

springRk4 :: (Floating a) => [a] -> [a] -> a -> [a]
springRk4 x u dt = rk4Step springDxdt x u dt

springA :: (Floating a) => [a] -> [a] -> [[a]]
springA = dA springDxdt

springB :: (Floating a) => [a] -> [a] -> [[a]]
springB = dB springDxdt

springCost :: (Floating a) => [a] -> [a] -> a
springCost x u = 7*position*position + 2*velocity*velocity + 4*force*force
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0

springQx :: (Floating a) => [a] -> [a] -> [a]
springQu :: (Floating a) => [a] -> [a] -> [a]
springQxx :: (Floating a) => [a] -> [a] -> [[a]]
springQuu :: (Floating a) => [a] -> [a] -> [[a]]
springQxu :: (Floating a) => [a] -> [a] -> [[a]]

springQx = qx springCost
springQu = qu springCost
springQxx = qxx springCost
springQuu = quu springCost
springQxu = qxu springCost


     
    
----
----quadraticApprox :: (Floating a, Mode s) => ([AD s a] -> AD s a) -> Quad b c d
------quadraticApprox :: (Floating a) => ([a] -> a) -> Quad b c d
------quadraticApprox f = Quad [[1]] [1] 1
----quadraticApprox f = Quad h g c
----  where
----    c = f x
    
--springCost' = 
--costQ = Quad

--qFcn x u v0 vx vxx = springCost x u + valueCost v0 vx vxx


--cost :: (Floating a) => [a] -> a
--cost x = 7*position*position + 2*velocity*velocity
--  where
--    position = x !! 0
--    velocity = x !! 1

--q x = grad cost x
--h x = hessian cost x
