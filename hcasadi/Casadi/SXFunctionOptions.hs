{-# OPTIONS_GHC -Wall #-}

module Casadi.SXFunctionOptions ( SXFunctionOption(..)
                                ) where

data SXFunctionOption = 
    Ad_mode String -- ^ (automatic) How to calculate the Jacobians: "forward" (only forward mode) "reverse" (only adjoint mode) or "automatic" (a heuristic decides which is more appropriate)
  | Inplace Bool -- ^ (False) Evaluate with inplace operations (experimental)
  | Jac_for_sens Bool -- ^ (False) Create the a Jacobian function and use this to calculate forward sensitivities
  | Just_in_time Bool -- ^ (False) Just-in-time compilation for numeric evaluation (experimental)
  | Live_variables Bool -- ^ (True) Reuse variables in the work vector
  | Monitor [String] -- ^ (???) Monitors to be activated
  | Name String -- ^ (unnamed_shared_object) n/a
  | Number_of_adj_dir Int -- ^ (1) number of adjoint derivatives to be calculated simultanously
  | Number_of_fwd_dir Int -- ^ (1) number of forward derivatives to be calculated simultanously
  | Numeric_hessian Bool -- ^ (False) Calculate Hessians numerically (using directional derivatives) rather than with the built-in method
  | Numeric_jacobian Bool -- ^ (False) Calculate Jacobians numerically (using directional derivatives) rather than with the built-in method
  | Sparse Bool -- ^ (True) function is sparse
  | Store_jacobians Bool -- ^ (False) keep references to generated Jacobians in order to avoid generating identical Jacobians multiple times
  | Topological_sorting String -- ^ (depth-first) Topological sorting algorithm
  | Verbose Bool -- ^ (False) verbose evaluation -- for debugging
