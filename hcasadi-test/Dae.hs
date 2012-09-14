{-# OPTIONS_GHC -Wall #-}

--module Main ( main ) where

import qualified Data.Traversable as T
import qualified Data.Vector.Storable as V

import Graphics.Gloss.Interface.IO.Simulate

import Dvda
import Casadi.Dvda
import Casadi.SXM
import Casadi.DAE
import Casadi.Integrator
import Casadi.IdasOptions

model :: (:*) DAEIn DAEOut (Maybe [Expr Double])
model = daeIn :* daeOut
  where -- differential states
    x = sym "x"
    z = sym "z"
    x' = sym "x'"
    z' = sym "z'"
    
    -- parameters
    m = sym "m"
--    r = 0.5
    g = 9.8

    -- external forces
    fx = 0
    fz = m*g

    -- dot(differential states)
    xDot = sym "xDot"
    zDot = sym "zDot"
    x'' = sym "x''"
    z'' = sym "z''"

    -- algebraic states
    lambda = sym "lambda"
    
    daeX = [x,z,x',z']
    daeZ = [lambda]
    daeP = [m]
    daeXDOT = [xDot,zDot,x'',z'']

    -- differential equations
    daeODE = [ m*x'' + x*lambda - fx
             , m*z'' + z*lambda - fz
             , x' - xDot
             , z' - zDot
             ]
    daeALG = [ x*x'' + z*z'' + (x'*x' + z'*z')
             ]

    daeIn = DAEIn { dae_X = Just daeX
                  , dae_Z = Just daeZ
                  , dae_P = Just daeP
                  , dae_T = Nothing
                  , dae_XDOT = Just daeXDOT
                  }

    daeOut = DAEOut { dae_ODE = Just daeODE
                    , dae_ALG = Just daeALG
                    , dae_QUAD = Nothing
                    }

main :: IO ()
main = do
  putStrLn "creating model"
  (daeIn:*daeOut) <- toCasadi model >>= T.traverse (T.traverse sxmVecCat)

  let stepsPerSecond = 100
  putStrLn "creating integrator"
  let integratorOptions = [ Reltol 1e-6
                          , Abstol 1e-8
                          , T0 0
                          , Tf (1/(fromIntegral stepsPerSecond))
                          ]
  int <- createIdasIntegrator daeIn daeOut [] integratorOptions

  let x0 = V.fromList [0.5/sqrt(2), 0.5/sqrt(2),0,0]
      p = V.fromList [1]
  putStrLn "running integrator"

  let draw (xvec,ts) = do
        let state@[x',z',_,_] = V.toList xvec
            r = 200
            x = realToFrac $ r*x'
            z = realToFrac $ -(r*z')
            heights = [180,160..]
        return $ Pictures $
          [ Color green $ Line [(0,0), (x,z)]
          , Color blue $ Translate x z $ Circle 10
          ] ++ zipWith3 (\h s n -> Color white $ Translate (-190) h $ Scale 0.1 0.1 $ Text (n ++ ": " ++ s)) heights
          (map show state ++ [show ts])
          ["x","z","vx","vz","timestep"]

      sim _ ts (x,_) = do
        xf <- int x (Just p)
        return (xf,ts)
  simulateIO (InWindow "woo" (400,400) (700,600)) black stepsPerSecond (x0,0) draw sim
