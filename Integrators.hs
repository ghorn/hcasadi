-- Integrators.hs

module Integrators( eulerStep
                  , rk4Step
                  ) where       

import Hom(State, Action, Ode)

eulerStep :: Floating a => Ode a -> State a -> Action a -> a -> State a
eulerStep dxdt x u dt = zipWith (+) x $ map (*dt) (dxdt x u)

rk4Step :: Floating a => Ode a -> State a -> Action a -> a -> State a
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
