module Main where

import Hom


sDxdt :: Floating a => Ode a
sDxdt state action = [v, (-k*x - b*v + u)/mass] where
  x    = state  !! 0
  v    = state  !! 1
  u    = action !! 0
  k    = 10
  b    = 0.1
  mass = 1


sEuler :: Floating a => State a -> Action a -> a -> State a
sEuler x u dt = eulerStep sDxdt x u dt


sRk4 :: Floating a => State a -> Action a -> a -> State a
sRk4 x u dt = rk4Step sDxdt x u dt


sA :: Floating a => State a -> Action a -> [[a]]
sA = dA sDxdt


sB :: Floating a => State a -> Action a -> [[a]]
sB = dB sDxdt


sCost :: Floating a => State a -> Action a -> a
sCost x u = 7*position*position + 2*velocity*velocity + 4*force*force
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0


sQx :: Floating a => State a -> Action a -> [a]
sQu :: Floating a => State a -> Action a -> [a]
sQxx :: Floating a => State a -> Action a -> [[a]]
sQuu :: Floating a => State a -> Action a -> [[a]]
sQxu :: Floating a => State a -> Action a -> [[a]]


sQx = qx sCost
sQu = qu sCost
sQxx = qxx sCost
sQuu = quu sCost
sQxu = qxu sCost
