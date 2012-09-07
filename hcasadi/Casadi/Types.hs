{-# OPTIONS_GHC -Wall #-}

module Casadi.Types ( SXFunction(..)
                    , NLPSolver(..)
                    ) where

import Foreign.ForeignPtr ( ForeignPtr )

import Casadi.Bindings.SXFunction ( SXFunctionRaw )
import Casadi.Bindings.NLPSolver ( NLPSolverRaw )

newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)
data NLPSolver = NLPSolver (ForeignPtr NLPSolverRaw)

