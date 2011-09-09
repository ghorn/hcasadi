-- cartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Xyz
import Quat
import Integrators(rk4Step)
import Vis
import Odes.Cartpole
import Casadi
import Ddp(prepareDdp)

type ControllerState = ([DMatrix], [DMatrix])

-- cost fcn
cost :: Matrix a b => a -> a -> b
cost state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u + barrier
  where
    [x, x', theta, theta'] = toList state
    [u] = toList action

    -- barrier
    uUb =  10.1
    uLb = -10.1
    mu = 1.0
    uBarrierUb = -mu*log(  uUb - u )
    uBarrierLb = -mu*log( -uLb + u )
    barrier = uBarrierUb + uBarrierLb

drawFun :: (DMatrix, ControllerState) -> IO ()
drawFun (state, (xTraj, uTraj)) = do
  let bobPath = VisLine (map cartpoleBob xTraj) (Rgb 1.0 0.1 0.1)
      axes = VisAxes (0.5, 5) (Xyz 0 0 0.5) (Quat 1 0 0 0)
      forceCylinder = cartpoleForceCylinder state (head uTraj)
  drawObjects $ [bobPath, cartpoleCart state, cartpoleCylinder state, cartpoleTrack, axes, forceCylinder]


-- discrete ode
dt :: Floating a => a
dt = 0.025

dode :: Matrix a b => a -> a -> a
dode x u = rk4Step cartpoleDxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 70
              x0 = fromList [-10,0,0.01,0::Double]
              u0 = fromList [0::Double]

              xTrajBadGuess = replicate n x0
              uTrajBadGuess = replicate n u0

              ddp = prepareDdp cost dode (4::Int) (1::Int) [(-10,10)]

              (xTraj, uTraj, _) = head $ drop 50 $ ddp xTrajBadGuess uTrajBadGuess

              simController x (xTrajPrev, uTrajPrev) = do
                let xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
                    uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
                    (xTraj', uTraj', _) = head $ ddp xTraj0 uTraj0
                    u = head uTrajPrev
                return (u, (xTraj', uTraj'))

          cartpoleVis simController drawFun (x0, (xTraj, uTraj)) dt
          print "hi"
