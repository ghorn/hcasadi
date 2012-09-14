{-# OPTIONS_GHC -Wall #-}

module Casadi.CvodesOptions ( CvodesOption(..)
                            ) where

data CvodesOption = 
    Abstol Double -- ^ (1e-08) n/a
  | Ad_mode String -- ^ (automatic) How to calculate the Jacobians: "forward" (only forward mode) "reverse" (only adjoint mode) or "automatic" (a heuristic decides which is more appropriate)
  | Asens_abstol Double -- ^ (???) n/a
  | Asens_iterative_solver String -- ^ (gmres) 
  | Asens_linear_solver String -- ^ (dense) 
  | Asens_lower_bandwidth Int -- ^ (???) n/a
  | Asens_max_krylov Int -- ^ (10) n/a
  | Asens_pretype String -- ^ (none) 
  | Asens_reltol Double -- ^ (???) n/a
  | Asens_upper_bandwidth Int -- ^ (???) n/a
  | Disable_internal_warnings Bool -- ^ (False) Disable CVodes internal warning messages
  | Exact_jacobian Bool -- ^ (False) n/a
  | Finite_difference_fsens Bool -- ^ (False) n/a
  | Fsens_abstol Double -- ^ (???) n/a
  | Fsens_all_at_once Bool -- ^ (True) n/a
  | Fsens_err_con Bool -- ^ (True) n/a
  | Fsens_reltol Double -- ^ (???) n/a
  | Fsens_scaling_factors [Double] -- ^ (???) n/a
  | Fsens_sensitiviy_parameters [Int] -- ^ (???) n/a
  | Interpolation_type String -- ^ (hermite) type of interpolation for the adjoint sensitivities
  | Iterative_solver String -- ^ (gmres) 
  | Jac_for_sens Bool -- ^ (False) Create the a Jacobian function and use this to calculate forward sensitivities
  | Linear_multistep_method String -- ^ (bdf) bdf|adams
  | Linear_solver String -- ^ (dense) 
  | Lower_bandwidth Int -- ^ (???) n/a
  | Max_krylov Int -- ^ (10) n/a
  | Max_multistep_order Int -- ^ (5) n/a
  | Max_num_steps Int -- ^ (10000) n/a
  | Monitor [String] -- ^ (???) Monitors to be activated
  | Name String -- ^ (unnamed_shared_object) n/a
  | Nonlinear_solver_iteration String -- ^ (newton) 
  | Nrhs Int -- ^ (1) n/a
  | Number_of_adj_dir Int -- ^ (1) number of adjoint derivatives to be calculated simultanously
  | Number_of_fwd_dir Int -- ^ (1) number of forward derivatives to be calculated simultanously
  | Numeric_hessian Bool -- ^ (False) Calculate Hessians numerically (using directional derivatives) rather than with the built-in method
  | Numeric_jacobian Bool -- ^ (False) Calculate Jacobians numerically (using directional derivatives) rather than with the built-in method
  | Pretype String -- ^ (none) 
  | Print_stats Bool -- ^ (False) Print out statistics after integration
  | Quad_err_con Bool -- ^ (False) n/a
  | Reltol Double -- ^ (1e-06) n/a
  | Sensitivity_method String -- ^ (simultaneous) 
  | Sparse Bool -- ^ (True) function is sparse
  | Steps_per_checkpoint Int -- ^ (20) n/a
  | Stop_at_end Bool -- ^ (False) n/a
  | Store_jacobians Bool -- ^ (False) keep references to generated Jacobians in order to avoid generating identical Jacobians multiple times
  | T0 Double -- ^ (0.0) n/a
  | Tf Double -- ^ (1.0) n/a
  | Upper_bandwidth Int -- ^ (???) n/a
  | Use_preconditioner Bool -- ^ (False) n/a
  | Verbose Bool -- ^ (False) verbose evaluation -- for debugging
