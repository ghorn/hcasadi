-- Cartpole.hs

{-# OPTIONS_GHC -Wall #-}

module Odes.Cartpole( cartpoleDxdt
                    , cartpoleBob
                    , cartpoleCylinder
                    , cartpoleCart
                    , cartpoleTrack
                    , cartpoleVis
                    , cartpoleForceCylinder
                    ) where

import Vis
import Xyz
import Quat
import Casadi
import Integrators(rk4Step)

import Graphics.UI.GLUT hiding (Matrix)
import Control.DeepSeq

cartpoleLength :: Floating a => a
cartpoleLength = 2.2

poleVisRadius :: Floating a => a
poleVisRadius = 0.03*cartpoleLength

cartVisRadius :: Floating a => a
cartVisRadius = 0.05*cartpoleLength

cartpoleDxdt :: Matrix a b c => a -> a -> a
cartpoleDxdt state action = state'
  where
    [_, x', theta, theta'] = toList state
    [u] = toList action
    
    -- constants
    g = 9.8;
    len = cartpoleLength
    mc = 2;
    mp = 1;

    x'' = 1/(mc+mp*sin(theta)*sin(theta))*(u+mp*sin(theta)*(len*theta'*theta'+g*cos(theta)))
    theta'' = 1/(len*(mc+mp*sin(theta)*sin(theta)))*(-u*cos(theta) - mp*len*theta'*theta'*cos(theta)*sin(theta) - (mc+mp)*g*sin(theta));

    state' = fromList [x', x'', theta', theta'']

cartpoleBob :: Fractional b => DMatrix -> Xyz b
cartpoleBob state = Xyz x' 0 z'
  where
    [x,_,theta,_] = toList state
    x' = realToFrac $ x + cartpoleLength*sin(theta)
    z' = realToFrac $ cartpoleLength*cos(theta)


cartpoleRod :: Fractional b => DMatrix -> Xyz b
cartpoleRod state = Xyz x' 0 z'
  where
    [x,_,theta,_]  = toList state
    x' = realToFrac $ x + 0.5*cartpoleLength*sin(theta)
    z' = realToFrac $ 0.5*cartpoleLength*cos(theta)


cartpoleCylinder :: DMatrix -> VisObject GLdouble GLfloat
cartpoleCylinder state = cylinder
  where
    [_,_,theta,_] = map realToFrac $ toList state

    quat = Quat (cos(0.5*theta)) 0.0 (sin(0.5*theta)) 0.0
    cylinder = VisCylinder (cartpoleLength, poleVisRadius) (cartpoleRod state) quat (Rgb 0 1 1)

cartpoleCart :: DMatrix -> VisObject GLdouble GLfloat
cartpoleCart state = VisBox (d,d,d) (Xyz x y z) (Quat 1 0 0 0) (Rgb 0 0 0.5)
  where
    d = 2*cartVisRadius
    
    x = realToFrac $ head (toList state)
    y = -poleVisRadius - cartVisRadius
    z = 0

cartpoleForceCylinder :: DMatrix -> DMatrix -> VisObject GLdouble GLfloat
cartpoleForceCylinder state action = VisCylinder (len, 0.1) (Xyz x0 y0 z0) quat (Rgb 1 1 0)
  where
    quat = Quat (sqrt(2)/2) 0 (signum(u)*sqrt(2)/2) 0

    u = realToFrac $ head (toList action)
    x = realToFrac $ head (toList state)

    len = u/4

    x0 = x + len/2
    y0 = -poleVisRadius - cartVisRadius
    z0 = -3*cartVisRadius

cartpoleTrack :: VisObject GLdouble GLfloat
cartpoleTrack = VisBox (dx,dy,dz) (Xyz x y z) (Quat 1 0 0 0) (Rgb 0.6 0 0)
  where
    dx = 10.0*cartpoleLength
    dy = 2*cartVisRadius
    dz = cartVisRadius
    
    x = 0
    y = -poleVisRadius - cartVisRadius
    z = cartVisRadius + (0.5*dz)

simFun :: (DMatrix -> DMatrix -> DMatrix)
          -> (DMatrix -> a -> IO (DMatrix, a))
          -> (DMatrix, a) -> IO (DMatrix, a)
simFun dode controller (x, controllerState) = do
  (u, newControllerState) <- controller x controllerState
  return (dode x u, newControllerState)

cartpoleVis :: (NFData a, Show a) => 
               (DMatrix -> a -> IO (DMatrix, a))
               -> ((DMatrix, a) -> IO ())
               -> (DMatrix, a)
               -> Double
               -> IO ()

cartpoleVis controller drawFun x0 dt = vis (simFun dode controller) drawFun x0 dt
  where
    dode x u = rk4Step cartpoleDxdt x u dt
