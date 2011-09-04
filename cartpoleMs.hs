-- cartpoleMs.hs

module Main where

import Vis
import MultipleShooting
import Casadi
import Odes.Cartpole
import Xyz
import Quat

import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Graphics.UI.GLUT

type ControllerState a = ([[a]], [[a]], [a])

-- ode
cartpoleOde :: Ode
cartpoleOde = Ode cartpoleDxdt' (4,1)
  where
    cartpoleDxdt' x u = fromList $ cartpoleDxdt (toList x) (toList u)

-- cost fcn
cpCost :: Cost
cpCost = Cost cpCost' (4,1)

cpCost' :: SXMatrix -> SXMatrix -> SX
cpCost' state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u
  where
    [x, x', theta, theta'] = toList state
    [u] = toList action


drawFun :: ([Double], ControllerState Double) -> IO ()
drawFun (state, (xTraj, uTraj, _)) = do
  let bobPath = VisLine (map cartpoleBob xTraj) (Rgb 1.0 0.1 0.1)
      axes = VisAxes (0.5, 5) (Xyz 0 0 0.5) (Quat 1 0 0 0)
      forceCylinder = cartpoleForceCylinder state (head uTraj)
  drawObjects $ [bobPath, cartpoleCart state, cartpoleCylinder state, cartpoleTrack, axes, forceCylinder]
  

simDt :: Floating a => a
simDt = 0.025
--simDt = 0.05


main :: IO ()
main = do
  let n = 60

      tEnd =  sxSymbolic "tEnd"
      dt = tEnd/(sxInt (n-1))

      sys = simpleSystem cartpoleOde cpCost dt n
      ms = multipleShooting sys (fromList [tEnd])

      x0 = [-10,0,0.01,0]
      xf = [0,0,pi,0]
      xBounds = [(-10,10), (-50,50), (-4*pi,4*pi), (-20*pi, 20*pi)]
  
      xBadGuess = (replicate ((rows $ designVars ms) - 1) (1.0::Double))++[5::Double]
  
      x0Sx = head $ states ms
      xfSx = last $ states ms
      xMiddle = init (tail $ states ms)
      stateBounds = concat $ map (\x -> boundIntervals ms x xBounds) (actions ms)
      actionBounds = concat $ map (\u -> boundIntervals ms u [(-10,10)]) (actions ms)
      bounds = concat [ boundEqs ms x0Sx x0
--                      , boundEqs ms xfSx xf
                      , stateBounds
                      , actionBounds
                      , [boundInterval ms tEnd (1, 5)]
--                      , [boundEq ms tEnd (fromIntegral(n-1)*simDt)]
                      ]

  msSolve <- multipleShootingSolver ms []
  (sol0,_) <- msSolve bounds xBadGuess
  print $ length sol0

  let simController x (xTrajPrev, uTrajPrev, paramsPrev) = do
        let bounds' = concat [ boundEqs ms x0Sx x
--                             , boundEqs ms xfSx xf
                             , stateBounds
                             , actionBounds
                             , [boundInterval ms tEnd (1,5)]
--                             , [boundEq ms tEnd (fromIntegral(n-1)*simDt)]
                             ]
            xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
            uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
            
            guess = concat (xTraj0 ++ uTraj0 ++ [paramsPrev])
            
        (sol, _) <- msSolve bounds' guess
        
        let ctrlState@(_, uTraj, _) = devectorize sol ms
            u = head uTrajPrev

        return (u, ctrlState)
  
  cartpoleVis simController drawFun (x0, devectorize sol0 ms) simDt
