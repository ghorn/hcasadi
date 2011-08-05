-- Hom.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Hom (State, Action, Ode, Cost,
            eulerStep, rk4Step,
            dA, dB,
            cx, cu, cxx, cuu, cxu,
            Quad(..), evalQuad) where

import Numeric.AD

type State a = [a]
type Action a = [a]
type Ode a = State a -> Action a -> State a
type Cost a = State a -> Action a -> a

--------------------- timesteps --------------------------
eulerStep :: (Floating a) => Ode a -> State a -> Action a -> a -> State a
eulerStep dxdt x u dt = zipWith (+) x $ map (*dt) (dxdt x u)

rk4Step :: (Floating a) => Ode a -> State a -> Action a -> a -> State a
rk4Step dxdt x u dt = zipWith (+) x $ map (/6) $ addLists [k1, twok2, twok3, k4]
  where
    addLists [] = []
    addLists (a:as) = foldl (\acc y -> zipWith (+) acc y) a as
    
    k1 = map (*dt) $ dxdt x u
    k2 = map (*dt) $ dxdt (addLists [x, map (*0.5) k1]) u
    k3 = map (*dt) $ dxdt (addLists [x, map (*0.5) k2]) u
    k4 = map (*dt) $ dxdt (addLists [x, k3])            u
    
    twok2 = map (*2) k2
    twok3 = map (*2) k3


-------- linearize dynamics f ~= f0 + dA*(x-x0) + dB*(u - u0) ---------
dA :: Floating a => (forall s b. (Floating b, Mode s) =>
                     [AD s b] -> [AD s b] -> [AD s b]) -> (State a -> Action a -> [[a]])
dA f x u = jacobian g x
  where
    g x' = f x' (map lift u)


dB :: Floating a => (forall s b. (Floating b, Mode s) =>
                     [AD s b] -> [AD s b] -> [AD s b]) -> (State a -> Action a -> [[a]])
dB f x u = jacobian g u
  where
    g u' = f (map lift x) u'


-------------- quadratic expansion of cost(x,u) ------------------
cx :: Floating a => (forall s b. (Floating b, Mode s) => Cost (AD s b)) -> (State a -> Action a -> [a])
cu :: Floating a => (forall s b. (Floating b, Mode s) => Cost (AD s b)) -> (State a -> Action a -> [a])
cxx :: Floating a => (forall s b. (Floating b, Mode s) => Cost (AD s b)) -> (State a -> Action a -> [[a]])
cuu :: Floating a => (forall s b. (Floating b, Mode s) => Cost (AD s b)) -> (State a -> Action a -> [[a]])
cxu :: Floating a => (forall s b. (Floating b, Mode s) => Cost (AD s b)) -> (State a -> Action a -> [[a]])

cx cost x u = grad g x
  where
    g x' = cost x' (map lift u)

cu cost x u = grad g u
  where
    g u' = cost (map lift x) u'

cxx cost x u = hessian g x
  where
    g x' = cost x' (map lift u)

cuu cost x u = hessian g u
  where
    g u' = cost (map lift x) u'

cxu cost x u = jacobian g u
  where
    g u' = (cx cost) (map lift x) u'


------------- quadratic data type -----------
data Quad a = Quad [[a]] [a] a [a] deriving (Show)

evalQuad :: Floating a => Quad a -> [a] -> a
evalQuad (Quad h g a x0) x = constTerm + linearTerm + quadraticTerm
   where
     constTerm = a
     linearTerm = dotLists g x' -- put length check here
     quadraticTerm = (*0.5) $ dotLists x' $ map (\y -> dotLists y x') h -- put length check here

     dotLists :: Floating a => [a] -> [a] -> a
     dotLists xa xb = sum (zipWith (*) xa xb)

     x' = zipWith (-) x x0
