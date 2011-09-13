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
import Casadi

import Graphics.UI.GLUT hiding (Matrix)
import Control.DeepSeq

poleVisRadius :: (Floating a, Fractional a) => a
poleVisRadius = 0.1*l1

cartVisRadius :: (Floating a, Fractional a) => a
cartVisRadius = 0.1*l1

l1 :: (Floating a, Fractional a) => a
l1 = 0.1
l2 :: (Floating a, Fractional a) => a
l2 = 0.1


doubleCartpoleDxdt :: Matrix a b c => a -> a -> a
doubleCartpoleDxdt state action = fromList [x0d, x1d, x2d, x0dd, x1dd, x2dd] where

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
    
  [_, x1, x2, x0d, x1d, x2d] = toList state
  [f] = toList action
    
  x0dd = (-50*f*(-3 + cos(2*(x1 - x2))) + (2*(x1d*x1d) - 200*cos(x1) + (x2d*x2d)*cos(x1 - x2))*sin(x1))/(10*(-4 + cos(2*x1) + cos(2*(x1 - x2))));
  x1dd = (-100*f*(-3*cos(x1) + cos(x1 - 2*x2)) - 1000*sin(x1) - 200*sin(x1 - 2*x2) + 4*(x1d*x1d)*cos(x2)*sin(2*x1 - x2) + (x2d*x2d)*(5*sin(x1 - x2) + sin(x1 + x2)))/(2*(-4 + cos(2*x1) + cos(2*(x1 - x2))));
  x2dd = (-2*(-200*cos(x1) + (x2d*x2d)*cos(x1 - x2) + 2*((x1d*x1d) - 50*f*sin(x1)))*sin(x1 - x2))/(-4 + cos(2*x1) + cos(2*(x1 - x2)));


doubleCartpoleCart :: DMatrix -> VisObject GLdouble GLfloat
doubleCartpoleCart state = VisBox (d,d,d) (xyzRealToFrac $ cartXyz state) (Quat 1 0 0 0) (Rgb 0 0 0.5)
  where
    d = 2*cartVisRadius

cartXyz :: Matrix a b c => a -> Xyz b
cartXyz state = Xyz x y z
  where
    (x:_) = toList state
    y = -poleVisRadius - cartVisRadius
    z = 0

rod0Xyz :: Matrix a b c => a -> Xyz b
rod0Xyz state = (Xyz cartX 0 cartZ) + r_c2r0_n
  where
    Xyz cartX _ cartZ = cartXyz state
    (_:q1:_) = toList state
    r_c2r0_n = Xyz (-0.5*l1*sin(q1)) 0 (-0.5*l1*cos(q1))
    
bob0Xyz :: Matrix a b c => a -> Xyz b
bob0Xyz state = (Xyz cartX 0 cartZ) + r_c2b0_n
  where
    Xyz cartX _ cartZ = cartXyz state
    (_:q1:_) = toList state
    r_c2b0_n = Xyz (-l1*sin(q1)) 0 (-l1*cos(q1))

rod1Xyz :: Matrix a b c => a -> Xyz b
rod1Xyz state = (bob0Xyz state) + r_b02r1_n
  where
    (_:_:q2:_) = toList state
    r_b02r1_n = Xyz (-0.5*l2*sin(q2)) 0 (-0.5*l2*cos(q2))

bob1Xyz :: Matrix a b c => a -> Xyz b
bob1Xyz state = (bob0Xyz state) + r_b02b1_n
  where
    (_:_:q2:_) = toList state
    r_b02b1_n = Xyz (-l2*sin(q2)) 0 (-l2*cos(q2))

doubleCartpoleCylinder0 :: DMatrix -> VisObject GLdouble GLfloat
doubleCartpoleCylinder0 state = cylinder
  where
    (_:q1:_) = map realToFrac $ toList state

    quat = Quat (cos(0.5*q1)) 0.0 (sin(0.5*q1)) 0.0
    cylinder = VisCylinder (l1, poleVisRadius) (xyzRealToFrac $ rod0Xyz state) quat (Rgb 0 1 1)

doubleCartpoleCylinder1 :: DMatrix -> VisObject GLdouble GLfloat
doubleCartpoleCylinder1 state = cylinder
  where
    (_:_:q2:_) = map realToFrac $ toList state

    quat = Quat (cos(0.5*q2)) 0.0 (sin(0.5*q2)) 0.0
    cylinder = VisCylinder (l1, poleVisRadius) (xyzRealToFrac $ rod1Xyz state) quat (Rgb 0 1 0.3)

doubleCartpoleTrack :: VisObject GLdouble GLfloat
doubleCartpoleTrack = VisBox (dx,dy,dz) (Xyz x y z) (Quat 1 0 0 0) (Rgb 0.6 0 0)
  where
    dx = 100.0*cartVisRadius
    dy = 2*cartVisRadius
    dz = cartVisRadius
    
    x = 0
    y = -poleVisRadius - cartVisRadius
    z = cartVisRadius + (0.5*dz)



doubleCartpoleForceCylinder :: DMatrix -> DMatrix -> VisObject GLdouble GLfloat
doubleCartpoleForceCylinder state action = VisCylinder (len, 0.01) (Xyz x0 y0 z0) quat (Rgb 1 1 0)
  where
    quat = Quat (sqrt(2)/2) 0 (-signum(u)*sqrt(2)/2) 0

    u = -(realToFrac $ head $ toList action)
    x = realToFrac $ head $ toList state

    len = u/4

    x0 = x + len/2
    y0 = -poleVisRadius - cartVisRadius
    z0 = -3*cartVisRadius


simFun :: (DMatrix -> DMatrix -> DMatrix)
          -> ((Maybe SpecialKey) -> DMatrix -> a -> IO (DMatrix, a))
          -> (Maybe SpecialKey) -> (DMatrix, DMatrix, a) -> IO (DMatrix, DMatrix, a)
simFun dode controller key (xOld, uOld, controllerStateOld) = do
  let xNew = dode xOld uOld
  (uNew, controllerStateNew) <- controller key xNew controllerStateOld
  return (xNew, uNew, controllerStateNew)


doubleCartpoleVis :: (NFData a, Show a) => 
                     ((Maybe SpecialKey) -> DMatrix -> a -> IO (DMatrix, a))
                     -> ((DMatrix, DMatrix, a) -> IO ())
                     -> (DMatrix, DMatrix, a)
                     -> Double
                     -> Double
                     -> IO ()
doubleCartpoleVis controller drawFun xuc0 simDt timeDialationFactor = visOut
  where
    visOut = vis camera0 (simFun dode controller) drawFun xuc0 (simDt/timeDialationFactor)
    dode x u = rk4Step doubleCartpoleDxdt x u simDt
    camera0 = Camera0 { phi0 = 60
                      , theta0 = 20
                      , rho0 = 0.9}
