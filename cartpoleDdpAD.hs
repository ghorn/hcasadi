-- cartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Ddp(ddp)
import Vis
import Odes.Cartpole

type SimState a = (State a, [State a], [Action a])

  
drawFun :: SimState Double -> IO ()
drawFun (x, xTraj, _) = do
  let bobPath = VisLine (map cartpoleBob xTraj) (Rgb 1.0 0.1 0.1)
      axes = VisAxes (0.5, 5) (Xyz 0 0 0.5) (Quat 1 0 0 0)
  drawObjects $ [bobPath, cartpoleCart x, cartpoleCylinder x, cartpoleTrack, axes]


simFun :: SimState Double -> IO (SimState Double)
simFun (x, xTraj0, uTraj0) = do return (dode x u, xTraj, uTraj)
  where
    xTraj0' = x:(drop 2 xTraj0) ++ [last xTraj0]
    uTraj0' = (tail uTraj0) ++ [last uTraj0]
    (xTraj, uTraj, _) = head $ ddp cost dode xTraj0' uTraj0'
    u = head uTraj

-- cost fcn
cost :: Floating a => State a -> Action a -> a
cost state action = 10*x*x + x'*x' + 100*(theta-pi)*(theta-pi) + theta'*theta' + 0.01*u*u
--cost state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u
  where
    [x, x', theta, theta'] = state
    [u] = action

-- discrete ode
dt :: Floating a => a
dt = 0.025

dode :: Floating a => State a -> Action a -> State a
dode x u = rk4Step cartpoleDxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 100
              x0 = [0,0,3,0::Double]
              u0 = [0::Double]
              
              xTraj0 = replicate n x0
              uTraj0 = replicate n u0
              
              (xTraj, uTraj, _) = head $ drop 50 $ ddp cost dode xTraj0 uTraj0

          vis simFun drawFun (x0, xTraj, uTraj) dt
