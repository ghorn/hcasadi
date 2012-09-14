{-# OPTIONS_GHC -Wall #-}

--module Main ( main ) where

import qualified Data.Traversable as T
import qualified Data.Vector.Storable as V

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

    -- lagrange multiplier
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
    daeALG = [ x*x'' + z*z'' + (x'*x' + z'*z') ]

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
  putStrLn "creating integrator"
  let integratorOptions = [ Reltol 1e-4
                          , Abstol 1e-6
                          , T0 0
                          ]
  int <- createIdasIntegrator daeIn daeOut [] integratorOptions

  let x0 = V.fromList [1,2,3,4]
      p = V.fromList [1]
  putStrLn "running integrator"
  xf <- int x0 (Just p)
  print xf

--main :: IO ()
--main = do
--  let x = sym "x" :: Expr Double
--      y = sym "y"
--
--  putStrLn "creating function"
--  [x',y',xy'] <- toCasadi [x,y,x*y]
--  f <- sxFunctionCreateCallable [Just x', Just y'] [Just xy'] []
--  
--  putStrLn "calling function"
--  g <- f $ map Just [V.singleton 2, V.singleton 3]
--  print g
