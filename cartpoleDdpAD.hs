-- cartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Ddp(ddp)
import Vis
import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Graphics.UI.GLUT

type SimState a = (State a, [State a], [Action a])

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
                           ("rod_x", rod_x),
                           ("rod_y", rod_y),
                           ("bob_x", bob_x),
                           ("bob_y", bob_y),
                           ("len", len)]
      where
        bob_x = x + len*sin(theta)
        bob_y = len*cos(theta)
        rod_x = x + 0.5*len*sin(theta)
        rod_y = 0.5*len*cos(theta)
  
makeShapes :: State Double -> Action Double -> [VisShape GLdouble]
makeShapes x u = shapes
  where
    (_,outputs) = cartpole x u
    [_,_,theta,_] = map realToFrac x
    
    getOutput key = realToFrac $ fromJust $ DM.lookup key outputs
    rod_x = getOutput "rod_x"
    rod_y = getOutput "rod_y"
    len = getOutput "len"
    
    shapes = [((Cylinder' 0.1 len 10 10), (rod_x, 0, rod_y), (cos(0.5*theta),0,sin(0.5*theta),0), (0,1,1))]


drawFun :: SimState Double -> IO ()
drawFun (x, xTraj, uTraj) = do
  drawShapes (makeShapes x [0])
  let getOutput (x',u') key = realToFrac $ fromJust $ DM.lookup key (snd (cartpole x' u'))
      getOutputs key = map (\x' -> getOutput x' key) $ tail $ zip xTraj uTraj
    
      bob_x = getOutputs "bob_x"
      bob_y = getOutputs "bob_y"

      bobPath :: [(GLfloat, GLfloat, GLfloat)]
      bobPath = zipWith (\a b -> (a,0,b)) bob_x bob_y
  
  preservingMatrix $ do
    materialDiffuse Front $= Color4 1 0.1 0.1 1
    color (Color3 1 0.1 0.1 :: Color3 GLfloat)
    renderPrimitive LineStrip $ mapM_ (\(a,b,c)->vertex$Vertex3 a b c) bobPath


simFun :: SimState Double -> SimState Double
simFun (x, xTraj0, uTraj0) = (dode x u, xTraj, uTraj)
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
dode x u = rk4Step dxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 100
              x0 = [0,0,3,0::Double]
              u0 = [0::Double]
              
              xTraj0 = replicate n x0
              uTraj0 = replicate n u0
              
              (xTraj, uTraj, _) = head $ drop 50 $ ddp cost dode xTraj0 uTraj0

          vis simFun drawFun (x0, xTraj, uTraj) dt
