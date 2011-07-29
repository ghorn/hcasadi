-- unitTests.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module UnitTests where

import Hom
import Ddp

-- spring ode
sDt :: Floating a => a
sDt = 0.025

sDxdt :: Floating a => Ode a
sDxdt state action = [v, (-k*x - b*v + u)/mass]
  where
    x    = state  !! 0
    v    = state  !! 1
    u    = action !! 0
    k    = 10
    b    = 0.8
    mass = 1

-- spring cost fcn
sCost :: Floating a => State a -> Action a -> a
sCost x u = (7*position*position + 2*velocity*velocity + 0.2*force*force)
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0

-- euler step
sEuler :: Floating a => State a -> Action a -> a -> State a
sEuler x u dt = eulerStep sDxdt x u dt

-- rk4 step
sRk4 :: Floating a => State a -> Action a -> a -> State a
sRk4 x u dt = rk4Step sDxdt x u dt

-- dynamics linearizations dA/dB
-- f ~= dA*(x-x0) + dB*(u-u0) + f0
sA :: Floating a => State a -> Action a -> [[a]]
sA = dA sDxdt

sB :: Floating a => State a -> Action a -> [[a]]
sB = dB sDxdt

-- cost fcn quadratic expansion
sCx :: Floating a => State a -> Action a -> [a]
sCu :: Floating a => State a -> Action a -> [a]
sCxx :: Floating a => State a -> Action a -> [[a]]
sCuu :: Floating a => State a -> Action a -> [[a]]
sCxu :: Floating a => State a -> Action a -> [[a]]

sCx = cx sCost
sCu = cu sCost
sCxx = cxx sCost
sCuu = cuu sCost
sCxu = cxu sCost

-- q fcn quadratic expansion
sQ0  :: Floating a => State a -> Action a -> Quad a -> a
sQx  :: Floating a => State a -> Action a -> Quad a -> [a]
sQu  :: Floating a => State a -> Action a -> Quad a -> [a]
sQxx :: Floating a => State a -> Action a -> Quad a -> [[a]]
sQuu :: Floating a => State a -> Action a -> Quad a -> [[a]]
sQxu :: Floating a => State a -> Action a -> Quad a -> [[a]]

sQ0  = q0  sCost (\x u -> sRk4 x u sDt)
sQx  = qx  sCost (\x u -> sRk4 x u sDt)
sQu  = qu  sCost (\x u -> sRk4 x u sDt)
sQxx = qxx sCost (\x u -> sRk4 x u sDt)
sQuu = quu sCost (\x u -> sRk4 x u sDt)
sQxu = qxu sCost (\x u -> sRk4 x u sDt)
