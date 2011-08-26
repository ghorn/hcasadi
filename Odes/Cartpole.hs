-- Cartpole.hs

{-# OPTIONS_GHC -Wall #-}

module Odes.Cartpole( cartpoleDxdt
                    , cartpoleOutputs
                    , cartpoleVis
                    , makeShapes
                    ) where

import Vis
import Hom(rk4Step)

import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Graphics.UI.GLUT
import Control.DeepSeq

cartpoleLength :: Floating a => a
cartpoleLength = 2.2

cartpoleDxdt :: Floating a => [a] -> [a] -> [a]
cartpoleDxdt state action = state'
  where
    [_, x', theta, theta'] = state
    [u] = action
    
    -- constants
    g = 9.8;
    len = cartpoleLength
    mc = 2;
    mp = 1;

    x'' = 1/(mc+mp*sin(theta)*sin(theta))*(u+mp*sin(theta)*(len*theta'*theta'+g*cos(theta)))
    theta'' = 1/(len*(mc+mp*sin(theta)*sin(theta)))*(-u*cos(theta) - mp*len*theta'*theta'*cos(theta)*sin(theta) - (mc+mp)*g*sin(theta));

    state' = [x', x'', theta', theta'']

cartpoleOutputs :: Floating a => [a] -> [a] -> DM.Map String a
cartpoleOutputs state _ = DM.fromList [("cart_x", x),
                                       ("cart_y", 0),
                                       ("rod_x", rod_x),
                                       ("rod_y", rod_y),
                                       ("bob_x", bob_x),
                                       ("bob_y", bob_y),
                                       ("len", len)]
  where
    [x,_,theta,_] = state
    len = cartpoleLength
    bob_x = x + len*sin(theta)
    bob_y = len*cos(theta)
    rod_x = x + 0.5*len*sin(theta)
    rod_y = 0.5*len*cos(theta)

makeShapes :: [Double] -> [Double] -> [VisShape GLdouble]
makeShapes x u = shapes
  where
    outputs = cartpoleOutputs x u
    [_,_,theta,_] = map realToFrac x
    
    getOutput key = realToFrac $ fromJust $ DM.lookup key outputs
    rod_x = getOutput "rod_x"
    rod_y = getOutput "rod_y"
    len = getOutput "len"
    
    shapes = [((Cylinder' 0.1 len 10 10), (rod_x, 0, rod_y), (cos(0.5*theta),0,sin(0.5*theta),0), (0,1,1))]


simFun :: ([Double] -> [Double] -> [Double])
          -> ([Double] -> a -> IO ([Double], a))
          -> ([Double], a) -> IO ([Double], a)
simFun dode controller (x, controllerState) = do
  (u, newControllerState) <- controller x controllerState
  return (dode x u, newControllerState)

cartpoleVis :: (NFData a, Show a) => 
               ([Double] -> a -> IO ([Double], a))
               -> (([Double], a) -> IO ())
               -> ([Double], a)
               -> Double
               -> IO ()

cartpoleVis controller drawFun x0 dt = vis (simFun dode controller) drawFun x0 dt
  where
    dode x u = rk4Step cartpoleDxdt x u dt
