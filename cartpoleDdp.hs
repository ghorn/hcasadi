-- cartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Ddp(ddp)
import Vis
import Graphics.Gnuplot.Simple
import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Graphics.UI.GLUT

-- ode
dxdt :: Floating a => Ode a
dxdt state action = state'
  where
    (state',_) = cartpole state action
  
cartpole :: Floating a => State a -> Action a -> (State a, DM.Map String a)
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

    outputs = DM.fromList [("cart_x", x),
                           ("cart_y", 0),
                           ("bob_x", bob_x),
                           ("bob_y", bob_y),
                           ("len", len)]
      where
        bob_x = x + 0.5*len*sin(theta)
        bob_y = 0.5*len*cos(theta)
  
makeShapes :: State Double -> Action Double -> [VisShape GLdouble]
makeShapes x u = shapes
  where
    (_,outputs) = cartpole x u
    [_,_,theta,_] = map realToFrac x
    
    getOutput key = realToFrac $ fromJust $ DM.lookup key outputs
    bob_x = getOutput "bob_x"
    bob_y = getOutput "bob_y"
    len = getOutput "len"
    
    shapes = [((Cylinder' 0.1 len 10 10), (bob_x, 0, bob_y), (cos(0.5*theta),0,sin(0.5*theta),0), (0,1,1))]

simFun :: State Double -> State Double
simFun x = dode x [0]

drawFun :: State Double -> IO ()
drawFun x = do
  drawShapes (makeShapes x [0])

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
              x0 = [0,0,3.1,0::Double]
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
              
          vis simFun drawFun x0 dt
          
--          plotLists [] $ map (zip time) [x, x', theta, theta', u]
--          print $ "total cost: " ++ (show (sum (map (\(xi,ui) -> cost xi ui) (zip xTraj uTraj))))
