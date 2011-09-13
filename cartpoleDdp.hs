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
import Graphics.UI.GLUT(SpecialKey(..))

type ControllerState = ([DMatrix], [DMatrix])

-- cost fcn
cost :: Matrix a b c => a -> a -> a
cost state action = fromList [x*x
                              + 0.01*x'*x'
                              + 100*cos(theta)
                              + 0.01*theta'*theta'
                              + 0.0001*u*u
                              + barrier]
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
dt = 0.05

dode :: Matrix a b c => a -> a -> a
dode x u = rk4Step cartpoleDxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 100
              alpha = 0.01
              x0 = fromList [-4,0,0.01,0]

              xTrajBadGuess = replicate n x0
              uTrajBadGuess = replicate n (fromList [0])

              uLbs = fromList [-10]
              uUbs = fromList [10]

              ddp = prepareDdp ("cartpoleDdp_n"++show n) cost dode (4,1) n (uLbs, uUbs)

              (xTraj, uTraj) = head $ drop 50 $ ddp alpha xTrajBadGuess uTrajBadGuess

              simController key x (xTrajPrev, uTrajPrev) = do
                let xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
                    uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
                    (xTraj', uTraj') = ddp alpha xTraj0 uTraj0 !! 1
                    u0 = head uTraj'
                    u = case key of Just KeyRight -> u0 + fromList [10]
                                    Just KeyLeft  -> u0 - fromList [10]
                                    Just KeyDown  -> fromList [0]
                                    _             -> u0
                return (u, (xTraj', uTraj'))

          cartpoleVis simController drawFun (x0, (xTraj, uTraj)) dt
          print "hi"
