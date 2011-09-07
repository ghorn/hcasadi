-- doubleCartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Xyz
import Quat
import Integrators(rk4Step)
import Vis
import Odes.DoubleCartpole
import DdpCasadi(prepareDdp)

type ControllerState a = ([State a], [Action a])

-- cost fcn
cost :: Floating a => State a -> Action a -> a
--cost state action = 10*x*x + x'*x' - 100*cos(q1) - 100*cos(q2) + q1'*q1' + q2'*q2' + 0.001*u*u + barrier
cost state action = 100*x*x
                    + 0.01*x'*x'
                    - 10*cos(q1)
                    - 10*cos(q2)
                    + 0.01*q1'*q1'
                    + 0.01*q2'*q2'
                    + 0.001*u*u
                    + barrier
  where
    [x,q1,q2,x',q1',q2'] = state
    [u] = action

    -- barrier
    uUb =  2.1
    uLb = -2.1
    mu = 1.0
    uBarrierUb = -mu*log(  uUb - u )
    uBarrierLb = -mu*log( -uLb + u )
    barrier = sum[ uBarrierUb, uBarrierLb ]

drawFun :: ([Double], ControllerState Double) -> IO ()
drawFun (state, (xTraj, uTraj)) = do
  let xyzToGLdouble (Xyz a b c) = Xyz (realToFrac a) (realToFrac b) (realToFrac c)
      bob0Path = VisLine (map (xyzToGLdouble . bob0Xyz) xTraj) (Rgb 1.0 0.1 0.1)
      bob1Path = VisLine (map (xyzToGLdouble . bob1Xyz) xTraj) (Rgb 1.0 0.1 0.1)
      axes = VisAxes (0.1, 5) (Xyz 0 0 0.1) (Quat 1 0 0 0)
      forceCylinder = doubleCartpoleForceCylinder state (head uTraj)
  drawObjects $ [ bob0Path
                , bob1Path
                , doubleCartpoleCart state
                , doubleCartpoleCylinder0 state
                , doubleCartpoleCylinder1 state
                , doubleCartpoleTrack, axes
                , forceCylinder
                ]


-- discrete ode
dt :: Floating a => a
dt = 0.01

dode :: Floating a => State a -> Action a -> State a
dode x u = rk4Step doubleCartpoleDxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 50
              x0 = [-0.2, 0.9*pi, 0.9*pi, 0, 0, 0::Double]
              u0 = [0::Double]

              xTrajBadGuess = replicate n x0
              uTrajBadGuess = replicate n u0

              ddp = prepareDdp cost dode (6::Int) (1::Int) [(-2,2)]

              (xTraj, uTraj, _) = head $ drop 50 $ ddp xTrajBadGuess uTrajBadGuess

              simController x (xTrajPrev, uTrajPrev) = do
                let xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
                    uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
                    (xTraj', uTraj', _) = head $ ddp xTraj0 uTraj0
                    u = head uTrajPrev
                return (u, (xTraj', uTraj'))

          doubleCartpoleVis simController drawFun (x0, (xTraj, uTraj)) dt
          print "hi"
