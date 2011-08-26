-- cartpoleMs.hs

module Main where

import MultipleShooting
import Casadi
import Odes.Cartpole

-- ode
cartpoleOde :: Ode
cartpoleOde = Ode cartpoleDxdt' (4,1)
  where
    cartpoleDxdt' x u = fromList $ cartpoleDxdt (toList x) (toList u)

-- cost fcn
cpCost :: Cost
cpCost = Cost cpCost' (4,1)

cpCost' :: SXMatrix -> SXMatrix -> SX
cpCost' state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u -- + barrier
  where
    [x, x', theta, theta'] = toList state
    [u] = toList action

--    -- barrier
--    uUb =  10.1
--    uLb = -10.1
--    mu = 1.0
--    uBarrierUb = -mu*log(  uUb - u )
--    uBarrierLb = -mu*log( -uLb + u )
--    barrier = uBarrierUb + uBarrierLb


main :: IO ()
main = do
  let n = 60
--      np = 1

      tEnd =  sxSymbolic "tEnd"
      dt = tEnd/(sxInt (n-1))

      sys = simpleSystem cartpoleOde cpCost dt n
      ms = multipleShooting sys (fromList [tEnd])

--      x0 = replicate n [-10,0,0.01,0::Double]
--      u0 = replicate n [0::Double]
      xGuess = (replicate ((rows $ designVars ms) - 1) (1.0::Double))++[10::Double]
  
      x0Sx = head $ states ms
      xfSx = last $ states ms
      
      bounds = concat [ boundEqs ms x0Sx [-10, 0, 0.01, 0]
                      , boundEqs ms xfSx [0,0,pi,0]
                      , [boundEq ms tEnd 10]
                      ]

  msSolve <- multipleShootingSolver ms []
  (sol0,_) <- msSolve bounds xGuess
  (sol,_) <- msSolve bounds sol0
  
  print $ length sol

