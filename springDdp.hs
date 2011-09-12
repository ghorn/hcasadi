-- springDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Integrators
import Casadi
import Ddp(prepareDdp)
import Graphics.Gnuplot.Simple

-- ode
sDxdt :: Matrix a b c => Ode a
sDxdt state action = fromList [v, (-k*x - b*v + u)/mass]
  where
    x    = toList state  !! 0
    v    = toList state  !! 1
    u    = toList action !! 0
    k    = 10
    b    = 0.8
    mass = 1

-- cost fcn
sCost :: Matrix a b c => a -> a -> b
sCost x u = (7*position*position + 2*velocity*velocity + 0.2*force*force)
  where
    position = toList x !! 0
    velocity = toList x !! 1
    force = toList u !! 0

-- discrete ode
sDt :: Floating a => a
sDt = 0.025

sDode :: Matrix a b c => a -> a -> a
sDode x u = rk4Step sDxdt x u sDt

-- run ddp
main :: IO ()
main = do let n = 100
              x0 = fromList [10,0 :: Double]
              u0 = fromList [0 :: Double]
              time :: [Double]
              time = take n [0,sDt..]
              
              xTraj0 = replicate n x0
              uTraj0 = replicate n u0
              cddp = prepareDdp sCost sDode (2::Int) (1::Int) [(-20,20)]

              (xTraj, uTraj, _) = head $ cddp xTraj0 uTraj0
              pos = map (\x -> (toList x) !! 0) xTraj
              vel = map (\x -> (toList x) !! 1) xTraj
              force = map (\x -> (toList x) !! 0) uTraj

          plotLists [] [zip time pos, zip time vel, zip time force]
          print $ "total cost:  " ++ (show (sum (map (\(x,u) -> sCost x u) (zip xTraj uTraj))))
