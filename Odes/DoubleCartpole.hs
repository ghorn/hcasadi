-- DoubleCartpole.hs

{-# OPTIONS_GHC -Wall #-}

module Odes.DoubleCartpole( doubleCartpoleDxdt
                          , doubleCartpoleVis
                          , doubleCartpoleCart
                          , doubleCartpoleCylinder0
                          , doubleCartpoleCylinder1
                          , doubleCartpoleForceCylinder
                          , doubleCartpoleTrack
                          , bob0Xyz
                          , bob1Xyz
                          ) where

import Vis
import Integrators(rk4Step)
import Xyz
import Quat

import Graphics.UI.GLUT
import Control.DeepSeq

poleVisRadius :: (Floating a, Fractional a) => a
poleVisRadius = 0.1*l1

cartVisRadius :: (Floating a, Fractional a) => a
cartVisRadius = 0.1*l1

l1 :: (Floating a, Fractional a) => a
l1 = 0.1
l2 :: (Floating a, Fractional a) => a
l2 = 0.1


doubleCartpoleDxdt :: Floating a => [a] -> [a] -> [a]
doubleCartpoleDxdt state action = [x0d, x1d, x2d, x0dd, x1dd, x2dd] where

--    g  = 9.8;
--    l1 = 0.1
--    l2 = 0.1
--    m0 = .1;
--    mp = .03;
--    m1 = mp;
--    m2 = mp;
    
--     d1 = m0+m1+m2;
--     d2 = (.5*m1+m2)*l1;
--     d3 = .5*m2*l2;
--     d4 = (m1/3+m2)*l1^2;
--     d5 = .5*m2*l1*l2;
--     d6 = m2*l2^2/3;
--     f1 = (.5*m1+m2)*l1*g;
--     f2 = .5*m2*l2*g;
    
--     th0  = y(1);
--     th1  = y(2);
--     th2  = y(3);
--     th0d = y(4);
--     th1d = y(5);
--     th2d = y(6);
-- 

    
--     D = [         d1,     d2*cos(th1),     d3*cos(th2);
--          d2*cos(th1),              d4, d5*cos(th1-th2);
--          d3*cos(th2), d5*cos(th1-th2),              d6;];
-- 
--     C = [0,     -d2*sin(th1)*th1d,    -d3*sin(th2)*th2d;
--          0,                     0, d5*sin(th1-th2)*th2d;
--          0, -d5*sin(th1-th2)*th1d,                    0;];
--     G = [0; -f1*sin(th1); -f2*sin(th2);];
-- 
--     H = [1;0;0;];

--     dy(4:6) = D\( - C*y(4:6) - G + H*u );
    
  [_, x1, x2, x0d, x1d, x2d] = state
  [f] = action
    
  x0dd = (-50*f*(-3 + cos(2*(x1 - x2))) + (2*(x1d*x1d) - 200*cos(x1) + (x2d*x2d)*cos(x1 - x2))*sin(x1))/(10*(-4 + cos(2*x1) + cos(2*(x1 - x2))));
  x1dd = (-100*f*(-3*cos(x1) + cos(x1 - 2*x2)) - 1000*sin(x1) - 200*sin(x1 - 2*x2) + 4*(x1d*x1d)*cos(x2)*sin(2*x1 - x2) + (x2d*x2d)*(5*sin(x1 - x2) + sin(x1 + x2)))/(2*(-4 + cos(2*x1) + cos(2*(x1 - x2))));
  x2dd = (-2*(-200*cos(x1) + (x2d*x2d)*cos(x1 - x2) + 2*((x1d*x1d) - 50*f*sin(x1)))*sin(x1 - x2))/(-4 + cos(2*x1) + cos(2*(x1 - x2)));


doubleCartpoleCart :: [Double] -> VisObject GLdouble GLfloat
doubleCartpoleCart state' = VisBox (d,d,d) (cartXyz state) (Quat 1 0 0 0) (Rgb 0 0 0.5)
  where
    state = map realToFrac state'
    d = 2*cartVisRadius

cartXyz :: (Floating a, Fractional a) => [a] -> Xyz a
cartXyz state = Xyz x y z
  where
    (x:_) = state
    y = -poleVisRadius - cartVisRadius
    z = 0

rod0Xyz :: (Floating a, Fractional a) => [a] -> Xyz a
rod0Xyz state = (Xyz cartX 0 cartZ) + r_c2r0_n
  where
    Xyz cartX _ cartZ = cartXyz state
    (_:q1:_) = state
    r_c2r0_n = Xyz (-0.5*l1*sin(q1)) 0 (-0.5*l1*cos(q1))
    
bob0Xyz :: (Floating a, Fractional a) => [a] -> Xyz a
bob0Xyz state = (Xyz cartX 0 cartZ) + r_c2b0_n
  where
    Xyz cartX _ cartZ = cartXyz state
    (_:q1:_) = state
    r_c2b0_n = Xyz (-l1*sin(q1)) 0 (-l1*cos(q1))

rod1Xyz :: (Floating a, Fractional a) => [a] -> Xyz a
rod1Xyz state = (bob0Xyz state) + r_b02r1_n
  where
    (_:_:q2:_) = state
    r_b02r1_n = Xyz (-0.5*l2*sin(q2)) 0 (-0.5*l2*cos(q2))

bob1Xyz :: (Floating a, Fractional a) => [a] -> Xyz a
bob1Xyz state = (bob0Xyz state) + r_b02b1_n
  where
    (_:_:q2:_) = state
    r_b02b1_n = Xyz (-l2*sin(q2)) 0 (-l2*cos(q2))

doubleCartpoleCylinder0 :: [Double] -> VisObject GLdouble GLfloat
doubleCartpoleCylinder0 state' = cylinder
  where
    state = map realToFrac state'
    (_:q1:_) = state

    quat = Quat (cos(0.5*q1)) 0.0 (sin(0.5*q1)) 0.0
    cylinder = VisCylinder (l1, poleVisRadius) (rod0Xyz state) quat (Rgb 0 1 1)

doubleCartpoleCylinder1 :: [Double] -> VisObject GLdouble GLfloat
doubleCartpoleCylinder1 state' = cylinder
  where
    state = map realToFrac state'
    (_:_:q2:_) = state

    quat = Quat (cos(0.5*q2)) 0.0 (sin(0.5*q2)) 0.0
    cylinder = VisCylinder (l1, poleVisRadius) (rod1Xyz state) quat (Rgb 0 1 0.3)

doubleCartpoleTrack :: VisObject GLdouble GLfloat
doubleCartpoleTrack = VisBox (dx,dy,dz) (Xyz x y z) (Quat 1 0 0 0) (Rgb 0.6 0 0)
  where
    dx = 100.0*cartVisRadius
    dy = 2*cartVisRadius
    dz = cartVisRadius
    
    x = 0
    y = -poleVisRadius - cartVisRadius
    z = cartVisRadius + (0.5*dz)



doubleCartpoleForceCylinder :: [Double] -> [Double] -> VisObject GLdouble GLfloat
doubleCartpoleForceCylinder state action = VisCylinder (len, 0.01) (Xyz x0 y0 z0) quat (Rgb 1 1 0)
  where
    quat = Quat (sqrt(2)/2) 0 (-signum(u)*sqrt(2)/2) 0

    u = realToFrac $ head action
    x = realToFrac $ head state

    len = u/4

    x0 = x + len/2
    y0 = -poleVisRadius - cartVisRadius
    z0 = -3*cartVisRadius


simFun :: ([Double] -> [Double] -> [Double])
          -> ([Double] -> a -> IO ([Double], a))
          -> ([Double], a) -> IO ([Double], a)
simFun dode controller (x, controllerState) = do
  (u, newControllerState) <- controller x controllerState
  return (dode x u, newControllerState)
--  return (dode x [0], controllerState)

doubleCartpoleVis :: (NFData a, Show a) => 
                     ([Double] -> a -> IO ([Double], a))
                     -> (([Double], a) -> IO ())
                     -> ([Double], a)
                     -> Double
                     -> IO ()
doubleCartpoleVis controller drawFun x0 dt = vis (simFun dode controller) drawFun x0 dt
  where
    dode x u = rk4Step doubleCartpoleDxdt x u dt