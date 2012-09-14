{-# OPTIONS_GHC -Wall #-}

module Casadi.IdasOptions ( IdasOption(..)
                          ) where
data IdasOption = 
    Abstol Double -- ^ (1e-08) n/a
  | Abstolv [Double] -- ^ (???) n/a
  | Ad_mode String -- ^ (automatic) How to calculate the Jacobians: "forward" (only forward mode) "reverse" (only adjoint mode) or "automatic" (a heuristic decides which is more appropriate)
  | Asens_abstol Double -- ^ (???) n/a
  | Asens_iterative_solver String -- ^ (gmres) 
  | Asens_linear_solver String -- ^ (dense) 
  | Asens_lower_bandwidth Int -- ^ (???) n/a
  | Asens_max_krylov Int -- ^ (10) n/a
  | Asens_pretype String -- ^ (none) 
  | Asens_reltol Double -- ^ (???) n/a
  | Asens_upper_bandwidth Int -- ^ (???) n/a
  | Calc_ic Bool -- ^ (True) use IDACalcIC to get consistent initial conditions. This only works for semi-explicit index-one systems. Else, you must provide consistent initial conditions yourself.
  | Calc_icb Bool -- ^ (False) use IDACalcIC to get consistent initial conditions. This only works for semi-explicit index-one systems. Else, you must provide consistent initial conditions yourself.
  | Cj_scaling Bool -- ^ (False) IDAS scaling on cj for the user-defined linear solver module
  | Disable_internal_warnings Bool -- ^ (False) Disable IDAS internal warning messages
  | Exact_jacobian Bool -- ^ (False) n/a
  | Extra_fsens_calc_ic Bool -- ^ (False) Call calc ic an extra time, with fsens=0
  | Finite_difference_fsens Bool -- ^ (False) n/a
  | First_time Double -- ^ (???) first requested time as a fraction of the time interval
  | Fsens_abstol Double -- ^ (???) n/a
  | Fsens_abstolv [Double] -- ^ (???) n/a
  | Fsens_err_con Bool -- ^ (True) n/a
  | Fsens_reltol Double -- ^ (???) n/a
  | Fsens_scaling_factors [Double] -- ^ (???) n/a
  | Fsens_sensitiviy_parameters [Int] -- ^ (???) n/a
  | Init_xdot [Double] -- ^ (???) Initial values for the state derivatives
  | Init_z [Double] -- ^ (???) Initial values for the algebraic states
  | Interpolation_type String -- ^ (hermite) type of interpolation for the adjoint sensitivities
  | Iterative_solver String -- ^ (gmres) 
  | Jac_for_sens Bool -- ^ (False) Create the a Jacobian function and use this to calculate forward sensitivities
  | Linear_solver String -- ^ (dense) 
  | Lower_bandwidth Int -- ^ (???) n/a
  | Max_krylov Int -- ^ (10) n/a
  | Max_multistep_order Int -- ^ (5) n/a
  | Max_num_steps Int -- ^ (10000) n/a
  | Max_step_size Double -- ^ (0) maximim step size
  | Monitor [String] -- ^ (???) Monitors to be activated
  | Name String -- ^ (unnamed_shared_object) n/a
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
  | Suppress_algebraic Bool -- ^ (False) supress algebraic variables in the error testing
  | T0 Double -- ^ (0.0) n/a
  | Tf Double -- ^ (1.0) n/a
  | Upper_bandwidth Int -- ^ (???) n/a
  | Use_preconditioner Bool -- ^ (False) n/a
  | Verbose Bool -- ^ (False) verbose evaluation -- for debugging
