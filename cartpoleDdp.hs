-- cartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Ddp
import Graphics.Gnuplot.Simple

-- ode
dxdt :: Floating a => Ode a
dxdt state action = state'
  where
    (state',_) = cartpole state action
  
cartpole :: Floating a => State a -> Action a -> (State a, [(String, a)])
cartpole state action = (state', outputs)
  where
    [x, x', theta, theta'] = state
    [u] = action
    
    -- constants
    g = 9.8;
    len = 2.2
    mc = 2;
    mp = 1;

    x'' = 1/(mc+mp*sin(theta)*sin(theta))*(u+mp*sin(theta)*(len*theta'*theta'+g*cos(theta)))
    theta'' = 1/(len*(mc+mp*sin(theta)*sin(theta)))*(-u*cos(theta) - mp*len*theta'*theta'*cos(theta)*sin(theta) - (mc+mp)*g*sin(theta));

    state' = [x', x'', theta', theta'']

    bob_x = x + len*sin(theta)
    bob_y = -len*cos(theta)
    outputs = [("cart_x", x),
               ("cart_y", 0),
               ("bob_x", bob_x),
               ("bob_y", bob_y)]
              
-- cost fcn
cost :: Floating a => State a -> Action a -> a
cost state action = x*x + x'*x' + theta*theta + theta'*theta' + u*u
  where
    [x, x', theta, theta'] = state
    [u] = action

-- discrete ode
dt :: Floating a => a
dt = 0.025

dode :: Floating a => State a -> Action a -> State a
dode x u = rk4Step dxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 100
              x0 = [1,2,3,4::Double]
              u0 = [0::Double]
              time :: [Double]
              time = take n [0,dt..]
              
              xTraj0 = replicate n x0
              uTraj0 = replicate n u0
              
              (xTraj, uTraj, _) = head $ ddp cost dode xTraj0 uTraj0

              x      = map (!! 0) xTraj
              x'     = map (!! 1) xTraj
              theta  = map (!! 2) xTraj
              theta' = map (!! 3) xTraj
              u      = map (!! 0) uTraj
          
          plotLists [] $ map (zip time) [x, x', theta, theta', u]
          print $ "total cost: " ++ (show (sum (map (\(xi,ui) -> cost xi ui) (zip xTraj uTraj))))
                                     

