-- Cartpole.hs

{-# OPTIONS_GHC -Wall #-}

module Odes.Cartpole( cartpoleDxdt
                    , cartpoleBob
                    , cartpoleCylinder
                    , cartpoleCart
                    , cartpoleTrack
                    , cartpoleVis
                    ) where

import Vis
import Integrators(rk4Step)

import Graphics.UI.GLUT
import Control.DeepSeq

cartpoleLength :: Floating a => a
cartpoleLength = 2.2

poleVisRadius :: Floating a => a
poleVisRadius = 0.03*cartpoleLength

cartVisRadius :: Floating a => a
cartVisRadius = 0.05*cartpoleLength

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

cartpoleBob :: (Real a, Floating a, Fractional b) => [a] -> Xyz b
cartpoleBob [x,_,theta,_] = Xyz x' 0 z'
  where x' = realToFrac $ x + cartpoleLength*sin(theta)
        z' = realToFrac $ cartpoleLength*cos(theta)
cartpoleBob _ = error "Wrong list length in cartpoleBob"


cartpoleRod :: (Real a, Floating a, Fractional b) => [a] -> Xyz b
cartpoleRod [x,_,theta,_] = Xyz x' 0 z'
  where x' = realToFrac $ x + 0.5*cartpoleLength*sin(theta)
        z' = realToFrac $ 0.5*cartpoleLength*cos(theta)
cartpoleRod _ = error "Wrong list length in cartpoleBob"


cartpoleCylinder :: [Double] -> VisObject GLdouble GLfloat
cartpoleCylinder x = cylinder
  where
    [_,_,theta,_] = map realToFrac x

    quat = Quat (cos(0.5*theta)) 0.0 (sin(0.5*theta)) 0.0
    cylinder = VisCylinder (cartpoleLength, poleVisRadius) (cartpoleRod x) quat (Rgb 0 1 1)

cartpoleCart :: [Double] -> VisObject GLdouble GLfloat
cartpoleCart state = VisBox (d,d,d) (Xyz x y z) (Quat 1 0 0 0) (Rgb 0 0 0.5)
  where
    d = 2*cartVisRadius
    
    x = realToFrac $ head state
    y = -poleVisRadius - cartVisRadius
    z = 0

cartpoleTrack :: VisObject GLdouble GLfloat
cartpoleTrack = VisBox (dx,dy,dz) (Xyz x y z) (Quat 1 0 0 0) (Rgb 0.6 0 0)
  where
    dx = 10.0*cartpoleLength
    dy = 2*cartVisRadius
    dz = cartVisRadius
    
    x = 0
    y = -poleVisRadius - cartVisRadius
    z = cartVisRadius + (0.5*dz)

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
