-- springDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Ddp
import Graphics.Gnuplot.Simple

-- ode
sDxdt :: Floating a => Ode a
sDxdt state action = [v, (-k*x - b*v + u)/mass]
  where
    x    = state  !! 0
    v    = state  !! 1
    u    = action !! 0
    k    = 10
    b    = 0.8
    mass = 1

-- cost fcn
sCost :: Floating a => State a -> Action a -> a
sCost x u = (7*position*position + 2*velocity*velocity + 0.2*force*force)
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0

-- discrete ode
sDt :: Floating a => a
sDt = 0.025

sDode :: Floating a => State a -> Action a -> State a
sDode x u = rk4Step sDxdt x u sDt

-- run ddp
main :: IO ()
main = do let n = 100
              x0 = [10,0]
              u0 = [0]
              time :: [Double]
              time = take n [0,sDt..]
              
              xTraj0 = replicate n x0
              uTraj0 = replicate n u0
              
              backsweepTrajectory :: [(Quad Double, [[Double]], [Double])]
              backsweepTrajectory = backSweep sCost sDode xTraj0 uTraj0

              forwardsweepTrajectory :: [(State Double, Action Double)]
              forwardsweepTrajectory = forwardSweep sDode x0 backsweepTrajectory
              (xTraj, uTraj) = unzip forwardsweepTrajectory

              pos = map (!! 0) xTraj
              vel = map (!! 1) xTraj
              force = map (!! 0) uTraj
          
          plotLists [] [zip time pos, zip time vel, zip time force]
          print $ "total cost: " ++ (show (sum (map (\(x,u) -> sCost x u) forwardsweepTrajectory)))
                                     

