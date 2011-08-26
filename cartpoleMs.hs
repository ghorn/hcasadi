-- cartpoleMs.hs

module Main where

import Vis(drawShapes)
import MultipleShooting
import Casadi
import Odes.Cartpole

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
drawFun (x, (xTraj, uTraj, _)) = do
  drawShapes (makeShapes x [0])
  let getOutput (x',u') key = realToFrac $ fromJust $ DM.lookup key (cartpoleOutputs x' u')
      getOutputs key = map (\x' -> getOutput x' key) $ tail $ zip xTraj uTraj

      bob_x = getOutputs "bob_x"
      bob_y = getOutputs "bob_y"

      bobPath :: [(GLfloat, GLfloat, GLfloat)]
      bobPath = zipWith (\a b -> (a,0,b)) bob_x bob_y

  preservingMatrix $ do
    materialDiffuse Front $= Color4 1 0.1 0.1 1
    color (Color3 1 0.1 0.1 :: Color3 GLfloat)
    renderPrimitive LineStrip $ mapM_ (\(a,b,c)->vertex$Vertex3 a b c) bobPath


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

      x0 = [0,0,0.01,0]
      xf = [0,0,pi,0]
      xBounds = [(-10,10), (-50,50), (-4*pi,4*pi), (-20*pi, 20*pi)]
  
      xBadGuess = (replicate ((rows $ designVars ms) - 1) (1.0::Double))++[3::Double]
  
      x0Sx = head $ states ms
      xfSx = last $ states ms
      xMiddle = init (tail $ states ms)
      stateBounds = concat $ map (\x -> boundIntervals ms x xBounds) (actions ms)
      actionBounds = concat $ map (\u -> boundIntervals ms u [(-10,10)]) (actions ms)
      bounds = concat [ boundEqs ms x0Sx x0
--                      , boundEqs ms xfSx xf
                      , stateBounds
                      , actionBounds
--                      , [boundInterval ms tEnd (2, 15)]
                      , [boundEq ms tEnd (fromIntegral(n-1)*simDt)]
                      ]

  msSolve <- multipleShootingSolver ms []
  (sol0,_) <- msSolve bounds xBadGuess
  print $ length sol0

  let simController x (xTrajPrev, uTrajPrev, paramsPrev) = do
        let bounds' = concat [ boundEqs ms x0Sx x
--                             , boundEqs ms xfSx xf
                             , stateBounds
                             , actionBounds
--                             , [boundInterval ms tEnd (2,15)]
                             , [boundEq ms tEnd (fromIntegral(n-1)*simDt)]
                             ]
            xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
            uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
            
            guess = concat (xTraj0 ++ uTraj0 ++ [paramsPrev])
            
        (sol, _) <- msSolve bounds' guess
        
        let ctrlState@(_, uTraj, _) = devectorize sol ms
            u = head uTrajPrev

        print u
        return (u, ctrlState)
  
  cartpoleVis simController drawFun (x0, devectorize sol0 ms) simDt
