-- doubleCartpoleMs.hs

--{-# OPTIONS_GHC -Wall #-}

module Main where

import Vis
import MultipleShooting
import Casadi
import Odes.DoubleCartpole
import Xyz
import Quat
import Graphics.UI.GLUT(SpecialKey(..))

type ControllerState = ([DMatrix], [DMatrix], DMatrix)

-- ode
doubleCartpoleOde :: Ode
doubleCartpoleOde = Ode doubleCartpoleDxdt (6,1)

-- cost fcn
cpCost :: Cost
cpCost = Cost cpCost' (6,1)

cpCost' :: SXMatrix -> SXMatrix -> SX
cpCost' state action = 100*x*x
                       + 0.01*x'*x'
                       - 10*cos(q1)
                       - 10*cos(q2)
                       + 0.01*q1'*q1'
                       + 0.01*q2'*q2'
                       + 0.001*u*u
  where
    [x,q1,q2,x',q1',q2'] = toList state
    [u] = toList action

cpCostFinal :: Cost
cpCostFinal = Cost cpCostFinal' (6,1)

cpCostFinal' :: SXMatrix -> SXMatrix -> SX
cpCostFinal' x u = 10*(cpCost' x u)


drawFun :: (DMatrix, ControllerState) -> IO ()
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
simDt = 0.005

timeDialationFactor :: Double
timeDialationFactor = 1.0 :: Double

main :: IO ()
main = do
  let n = 50
  
      tEnd =  sxSymbolic "tEnd"
      dt = tEnd/(sxInt (n-1))

      sys = replaceFinalCost cpCostFinal $ simpleSystem doubleCartpoleOde cpCost dt n
      ms = multipleShooting sys (fromList [tEnd])

      x0 = fromList [0, 0.9*pi, 0.9*pi, 0, 0, 0] :: DMatrix
      xf = fromList [0,0,0,0,0,0] :: DMatrix
      xBounds = [(-10,10), (-4*pi,4*pi), (-4*pi,4*pi), (-50,50), (-20*pi, 20*pi), (-20*pi, 20*pi)]
  
      xGuess = concat $ replicate n (toList x0)
      uGuess = concat $ replicate n [0]
      badGuess = fromList $ xGuess ++ uGuess ++ [1::Double]
  
      x0Sx = head $ states ms
      xfSx = last $ states ms
      xMiddle = init (tail $ states ms)
      stateBounds = concat $ map (\x -> boundIntervals ms x xBounds) (actions ms)
      actionBounds = concat $ map (\u -> boundIntervals ms u [(-1,1)]) (actions ms)
      bounds = concat [ boundEqs ms x0Sx x0
--                      , boundEqs ms xfSx xf
                      , stateBounds
                      , actionBounds
                      , [boundInterval ms tEnd (0.05, 0.5)]
                      ]

  msSolve <- multipleShootingSolver ms []
  (sol0,_) <- msSolve bounds badGuess
  print $ rows sol0

  let simController key x (xTrajPrev, uTrajPrev, paramsPrev) = do
        let bounds' = concat [ boundEqs ms x0Sx x
--                             , boundEqs ms xfSx xf
                             , stateBounds
                             , actionBounds
                             , [boundInterval ms tEnd (0.05, 0.5)]
                             ]
            xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
            uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
            
            guess = vertcat (xTraj0 ++ uTraj0 ++ [paramsPrev])
            
        (sol, _) <- msSolve bounds' guess
        
        let ctrlState@(_, uTraj, _) = devectorize sol ms
            u0 = head uTraj
            u = case key of Just KeyRight -> u0 + fromList [2]
                            Just KeyLeft  -> u0 - fromList [2]
                            Just KeyDown  -> fromList [2]
                            _             -> u0

        return (u, ctrlState)
  
  doubleCartpoleVis simController drawFun (x0, devectorize sol0 ms) simDt timeDialationFactor
