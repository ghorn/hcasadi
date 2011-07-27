-- spring.hs

module Main where

import Hom

-- spring ode
sDxdt :: Floating a => Ode a
sDxdt state action = [v, (-k*x - b*v + u)/mass] where
  x    = state  !! 0
  v    = state  !! 1
  u    = action !! 0
  k    = 10
  b    = 0.1
  mass = 1

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

-- cost fcn to test following expansions
sCost :: Floating a => State a -> Action a -> a
sCost x u = 7*position*position + 2*velocity*velocity + 4*force*force
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0

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
sQx  :: Floating a => State a -> Action a -> Quad a -> [a]
sQu  :: Floating a => State a -> Action a -> Quad a -> [a]
sQxx :: Floating a => State a -> Action a -> Quad a -> [[a]]
sQuu :: Floating a => State a -> Action a -> Quad a -> [[a]]
sQxu :: Floating a => State a -> Action a -> Quad a -> [[a]]

dtTestQ :: Floating a => a
dtTestQ = 0.1
sQx  = qx  sCost (\x u -> sEuler x u dtTestQ)
sQu  = qu  sCost (\x u -> sEuler x u dtTestQ)
sQxx = qxx sCost (\x u -> sEuler x u dtTestQ)
sQuu = quu sCost (\x u -> sEuler x u dtTestQ)
sQxu = qxu sCost (\x u -> sEuler x u dtTestQ)
