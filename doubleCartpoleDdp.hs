-- doubleCartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Xyz
import Quat
import Integrators(rk4Step)
import Vis
import Odes.DoubleCartpole
import Ddp(prepareDdp)
import Casadi
import Graphics.UI.GLUT(SpecialKey(..))

type ControllerState = ([DMatrix], [DMatrix])

-- cost fcn
cost :: Matrix a b c => a -> a -> a
cost state action = fromList [100*x*x
                              + 0.01*x'*x'
                              - 10*cos(q1)
                              - 10*cos(q2)
                              + 0.01*q1'*q1'
                              + 0.01*q2'*q2'
                              + 0.0004*u*u
                              + barrier]
  where
    [x,q1,q2,x',q1',q2'] = toList state
    [u] = toList action

    -- barrier
    uUb =  2.1
    uLb = -2.1
    mu = 1.0
    uBarrierUb = -mu*log(  uUb - u )
    uBarrierLb = -mu*log( -uLb + u )
    barrier = sum[ uBarrierUb, uBarrierLb ]

drawFun :: (DMatrix, ControllerState) -> IO ()
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

timeDialationFactor :: Double
timeDialationFactor = 0.3

dode :: Matrix a b c => a -> a -> a
dode x u = rk4Step doubleCartpoleDxdt x u dt


--"how would you derive a cost function from an end constraint xf?"
--"look at d^2/dxdu at optimal point?"
--"include noise?"

-- run ddp
main :: IO ()
main = do let n = 45
              alpha0 = 0.0
              alpha1 = 0.0

              x0 = fromList [-0.2, 0.9*pi, 0.9*pi, 0, 0, 0]

              xTrajBadGuess = replicate n x0
              uTrajBadGuess = replicate n (fromList [0])

              uLbs = fromList [-2]
              uUbs = fromList [2]
              ddp = prepareDdp ("doubleCartpoleDdp_n"++show n) cost dode (6,1) n (uLbs,uUbs)

              (xTraj, uTraj) = head $ drop 50 $ ddp alpha0 xTrajBadGuess uTrajBadGuess

              simController key x (xTrajPrev, uTrajPrev) = do
                let xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
                    uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
                    (xTraj', uTraj') = ddp alpha1 xTraj0 uTraj0 !! 3
                    u0 = head uTraj'
                    u = case key of Just KeyRight -> u0 + fromList [1]
                                    Just KeyLeft  -> u0 - fromList [1]
                                    Just KeyDown  -> fromList [0]
                                    _             -> u0
                
                return (u, (xTraj', uTraj'))

          doubleCartpoleVis simController drawFun (x0, (xTraj, uTraj)) dt timeDialationFactor
          print "hi"
