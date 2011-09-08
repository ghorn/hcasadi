-- Integrators.hs

module Integrators( eulerStep
                  , rk4Step
                  ) where       

import Casadi

eulerStep :: Matrix a b => (a -> a -> a) -> a -> a -> b -> a
eulerStep dxdt x u dt = x + (scale dt (dxdt x u))

rk4Step :: Matrix a b => (a -> a -> a) -> a -> a -> b -> a
rk4Step dxdt x u dt = x + (1/6)*(k1 + 2*k2 + 2*k3 + k4)
  where
    k1 = scale dt ( dxdt x            u )
    k2 = scale dt ( dxdt (x + 0.5*k1) u )
    k3 = scale dt ( dxdt (x + 0.5*k2) u )
    k4 = scale dt ( dxdt (x + k3)     u )
