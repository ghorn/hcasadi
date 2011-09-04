-- doubleCartpoleMs.hs

module Main where

import Vis
import MultipleShooting
import Casadi
import Odes.DoubleCartpole
import Xyz
import Quat

import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Graphics.UI.GLUT

type ControllerState a = ([[a]], [[a]], [a])

-- ode
doubleCartpoleOde :: Ode
doubleCartpoleOde = Ode doubleCartpoleDxdt' (6,1)
  where
    doubleCartpoleDxdt' x u = fromList $ doubleCartpoleDxdt (toList x) (toList u)

-- cost fcn
cpCost :: Cost
cpCost = Cost cpCost' (6,1)

cpCost' :: SXMatrix -> SXMatrix -> SX
cpCost' state action = 10*x*x
                       + 0.001*x'*x'
                       - 10*cos(q1)
                       - 10*cos(q2)
                       + 0.01*q1'*q1'
                       + 0.01*q2'*q2'
                       + 0.001*u*u
  where
    [x,q1,q2,x',q1',q2'] = toList state
    [u] = toList action


drawFun :: ([Double], ControllerState Double) -> IO ()
drawFun (state, (xTraj, uTraj, _)) = do
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

simDt :: Floating a => a
simDt = 0.01

main :: IO ()
main = do
  let n = 50

      tEnd =  sxSymbolic "tEnd"
      dt = tEnd/(sxInt (n-1))

      sys = simpleSystem doubleCartpoleOde cpCost dt n
      ms = multipleShooting sys (fromList [tEnd])

      x0 = [0, 0.9*pi, 0.9*pi, 0, 0, 0]
      xf = [0,0,0,0,0,0]
      xBounds = [(-10,10), (-4*pi,4*pi), (-4*pi,4*pi), (-50,50), (-20*pi, 20*pi), (-20*pi, 20*pi)]
  
      xGuess = concat $ replicate n x0
      uGuess = concat $ replicate n [0]
      badGuess = concat [xGuess,uGuess]++[1::Double]
  
      x0Sx = head $ states ms
      xfSx = last $ states ms
      xMiddle = init (tail $ states ms)
      stateBounds = concat $ map (\x -> boundIntervals ms x xBounds) (actions ms)
      actionBounds = concat $ map (\u -> boundIntervals ms u [(-1,1)]) (actions ms)
      bounds = concat [ boundEqs ms x0Sx x0
--                      , boundEqs ms xfSx xf
                      , stateBounds
                      , actionBounds
                      , [boundInterval ms tEnd (0.2, 1.0)]
--                      , [boundEq ms tEnd (fromIntegral(n-1)*simDt)]
                      ]

  msSolve <- multipleShootingSolver ms []
  (sol0,_) <- msSolve bounds badGuess
  print $ length sol0

  let simController x (xTrajPrev, uTrajPrev, paramsPrev) = do
        let bounds' = concat [ boundEqs ms x0Sx x
--                             , boundEqs ms xfSx xf
                             , stateBounds
                             , actionBounds
                             , [boundInterval ms tEnd (0.2, 1.0)]
--                             , [boundEq ms tEnd (fromIntegral(n-1)*simDt)]
                             ]
            xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
            uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
            
            guess = concat (xTraj0 ++ uTraj0 ++ [paramsPrev])
            
        (sol, _) <- msSolve bounds' guess
        
        let ctrlState@(_, uTraj, _) = devectorize sol ms
            u = head uTrajPrev

        return (u, ctrlState)
  
  doubleCartpoleVis simController drawFun (x0, devectorize sol0 ms) simDt
