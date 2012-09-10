{-# OPTIONS_GHC -Wall #-}

module Casadi.DAE ( DAEIn(..)
                  , DAEOut(..)
                  , ControlDAEIn(..)
                  , makeDae
                  , emptyDaeIn
                  , emptyDaeOut
                  , makeControlDae
                  , emptyControlDaeIn
                  ) where

import Casadi.SXM
import Casadi.SXFunction
import Casadi.SXFunctionOptions

data DAEIn = DAEIn { dae_X    :: Maybe SXM --Differential state [x].
                   , dae_Z    :: Maybe SXM --Algebraic state [z].
                   , dae_P    :: Maybe SXM --Parameter [p].
                   , dae_T    :: Maybe SXM --Explicit time dependence [t].
                   , dae_XDOT :: Maybe SXM --Time derivative of differential states [xdot].
                   }

data DAEOut = DAEOut { dae_ODE  :: Maybe SXM --Right hand side of the implicit ODE [ode].
                     , dae_ALG  :: Maybe SXM --Right hand side of algebraic equations [alg].
                     , dae_QUAD :: Maybe SXM --Right hand side of quadratures equations [quad].
                     }

data ControlDAEIn =
  ControlDAEIn
  { control_dae_T        :: Maybe SXM -- Global physical time. (1-by-1) [t].
  , control_dae_X        :: Maybe SXM -- State vector (dimension nx-by-1). Should have same amount of non-zeros as DAEOutput:DAE_RES [x].
  , control_dae_Z        :: Maybe SXM -- Algebraic state vector (dimension np-by-1). [z].
  , control_dae_P        :: Maybe SXM -- Parameter vector (dimension np-by-1). [p].
  , control_dae_U        :: Maybe SXM -- Control vector (dimension nu-by-1). [u].
  , control_dae_U_INTERP :: Maybe SXM -- Control vector, linearly interpolated (dimension nu-by-1). [u_interp].
  , control_dae_XDOT     :: Maybe SXM -- State derivative vector (dimension nx-by-1). Should have same amount of non-zeros as DAEOutput:DAE_RES [xdot].
  , control_dae_X_MAJOR  :: Maybe SXM -- State vector (dimension nx-by-1) at the last major time-step [x_major].
  , control_dae_T0       :: Maybe SXM -- Time at start of control interval (1-by-1) [t0].
  , control_dae_TF       :: Maybe SXM -- Time at end of control interval (1-by-1) [tf].
  }

makeDae :: DAEIn -> DAEOut -> [SXFunctionOption] -> IO SXFunction
makeDae daeIn daeOut options = sxFunctionCreate' inputs outputs options
  where
    inputs  = map (\x -> x daeIn ) [dae_X, dae_Z, dae_P, dae_T, dae_XDOT]
    outputs = map (\x -> x daeOut) [dae_ODE, dae_ALG, dae_QUAD]

makeControlDae :: ControlDAEIn -> SXM -> [SXFunctionOption] -> IO SXFunction
makeControlDae cdaeIn sysErr options = sxFunctionCreate' inputs [Just sysErr] options
  where
    inputs = map (\x -> x cdaeIn)
             [ control_dae_T
             , control_dae_X
             , control_dae_Z
             , control_dae_P
             , control_dae_U
             , control_dae_U_INTERP
             , control_dae_XDOT
             , control_dae_X_MAJOR
             , control_dae_T0
             , control_dae_TF
             ]

emptyDaeIn :: DAEIn
emptyDaeIn = DAEIn Nothing Nothing Nothing Nothing Nothing

emptyDaeOut :: DAEOut
emptyDaeOut = DAEOut Nothing Nothing Nothing

emptyControlDaeIn :: ControlDAEIn
emptyControlDaeIn = ControlDAEIn Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
