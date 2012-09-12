{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Casadi.DAE ( DAEIn(..)
                  , DAEOut(..)
                  , ControlDAEIn(..)
                  , emptyDaeIn
                  , emptyDaeOut
                  , emptyControlDaeIn
                  ) where

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )

data DAEIn a = DAEIn { dae_X    :: a --Differential state [x].
                     , dae_Z    :: a --Algebraic state [z].
                     , dae_P    :: a --Parameter [p].
                     , dae_T    :: a --Explicit time dependence [t].
                     , dae_XDOT :: a --Time derivative of differential states [xdot].
                     } deriving (Functor, Foldable, Traversable, Show)

data DAEOut a = DAEOut { dae_ODE  :: a --Right hand side of the implicit ODE [ode].
                       , dae_ALG  :: a --Right hand side of algebraic equations [alg].
                       , dae_QUAD :: a --Right hand side of quadratures equations [quad].
                       } deriving (Functor, Foldable, Traversable, Show)


data ControlDAEIn a =
  ControlDAEIn
  { control_dae_T        :: a -- Global physical time. (1-by-1) [t].
  , control_dae_X        :: a -- State vector (dimension nx-by-1). Should have same amount of non-zeros as DAEOutput:DAE_RES [x].
  , control_dae_Z        :: a -- Algebraic state vector (dimension np-by-1). [z].
  , control_dae_P        :: a -- Parameter vector (dimension np-by-1). [p].
  , control_dae_U        :: a -- Control vector (dimension nu-by-1). [u].
  , control_dae_U_INTERP :: a -- Control vector, linearly interpolated (dimension nu-by-1). [u_interp].
  , control_dae_XDOT     :: a -- State derivative vector (dimension nx-by-1). Should have same amount of non-zeros as DAEOutput:DAE_RES [xdot].
  , control_dae_X_MAJOR  :: a -- State vector (dimension nx-by-1) at the last major time-step [x_major].
  , control_dae_T0       :: a -- Time at start of control interval (1-by-1) [t0].
  , control_dae_TF       :: a -- Time at end of control interval (1-by-1) [tf].
  } deriving (Functor, Foldable, Traversable, Show)

emptyDaeIn :: DAEIn (Maybe a)
emptyDaeIn = DAEIn Nothing Nothing Nothing Nothing Nothing

emptyDaeOut :: DAEOut (Maybe a)
emptyDaeOut = DAEOut Nothing Nothing Nothing

emptyControlDaeIn :: ControlDAEIn (Maybe a)
emptyControlDaeIn = ControlDAEIn Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
