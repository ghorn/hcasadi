{-# OPTIONS_GHC -Wall #-}

module Casadi.IntegratorOptionsInternal ( idasUnsafeSetOption
                                        , cvodesUnsafeSetOption
                                        ) where

import qualified Casadi.IdasOptions as Idas
import qualified Casadi.CvodesOptions as Cvodes
import Casadi.Options ( Option(..) )
import Casadi.Types ( SXFunction(..) )

unsafeSetOption' :: Option a => SXFunction -> String -> a -> IO ()
unsafeSetOption' = unsafeSetOption

idasUnsafeSetOption :: SXFunction -> Idas.IdasOption -> IO ()
idasUnsafeSetOption fun (Idas.Abstol x) = unsafeSetOption' fun "abstol" x
idasUnsafeSetOption fun (Idas.Abstolv x) = unsafeSetOption' fun "abstolv" x
idasUnsafeSetOption fun (Idas.Ad_mode x) = unsafeSetOption' fun "ad_mode" x
idasUnsafeSetOption fun (Idas.Asens_abstol x) = unsafeSetOption' fun "asens_abstol" x
idasUnsafeSetOption fun (Idas.Asens_iterative_solver x) = unsafeSetOption' fun "asens_iterative_solver" x
idasUnsafeSetOption fun (Idas.Asens_linear_solver x) = unsafeSetOption' fun "asens_linear_solver" x
idasUnsafeSetOption fun (Idas.Asens_lower_bandwidth x) = unsafeSetOption' fun "asens_lower_bandwidth" x
idasUnsafeSetOption fun (Idas.Asens_max_krylov x) = unsafeSetOption' fun "asens_max_krylov" x
idasUnsafeSetOption fun (Idas.Asens_pretype x) = unsafeSetOption' fun "asens_pretype" x
idasUnsafeSetOption fun (Idas.Asens_reltol x) = unsafeSetOption' fun "asens_reltol" x
idasUnsafeSetOption fun (Idas.Asens_upper_bandwidth x) = unsafeSetOption' fun "asens_upper_bandwidth" x
idasUnsafeSetOption fun (Idas.Calc_ic x) = unsafeSetOption' fun "calc_ic" x
idasUnsafeSetOption fun (Idas.Calc_icb x) = unsafeSetOption' fun "calc_icB" x
idasUnsafeSetOption fun (Idas.Cj_scaling x) = unsafeSetOption' fun "cj_scaling" x
idasUnsafeSetOption fun (Idas.Disable_internal_warnings x) = unsafeSetOption' fun "disable_internal_warnings" x
idasUnsafeSetOption fun (Idas.Exact_jacobian x) = unsafeSetOption' fun "exact_jacobian" x
idasUnsafeSetOption fun (Idas.Extra_fsens_calc_ic x) = unsafeSetOption' fun "extra_fsens_calc_ic" x
idasUnsafeSetOption fun (Idas.Finite_difference_fsens x) = unsafeSetOption' fun "finite_difference_fsens" x
idasUnsafeSetOption fun (Idas.First_time x) = unsafeSetOption' fun "first_time" x
idasUnsafeSetOption fun (Idas.Fsens_abstol x) = unsafeSetOption' fun "fsens_abstol" x
idasUnsafeSetOption fun (Idas.Fsens_abstolv x) = unsafeSetOption' fun "fsens_abstolv" x
idasUnsafeSetOption fun (Idas.Fsens_err_con x) = unsafeSetOption' fun "fsens_err_con" x
idasUnsafeSetOption fun (Idas.Fsens_reltol x) = unsafeSetOption' fun "fsens_reltol" x
idasUnsafeSetOption fun (Idas.Fsens_scaling_factors x) = unsafeSetOption' fun "fsens_scaling_factors" x
idasUnsafeSetOption fun (Idas.Fsens_sensitiviy_parameters x) = unsafeSetOption' fun "fsens_sensitiviy_parameters" x
idasUnsafeSetOption fun (Idas.Init_xdot x) = unsafeSetOption' fun "init_xdot" x
idasUnsafeSetOption fun (Idas.Init_z x) = unsafeSetOption' fun "init_z" x
idasUnsafeSetOption fun (Idas.Interpolation_type x) = unsafeSetOption' fun "interpolation_type" x
idasUnsafeSetOption fun (Idas.Iterative_solver x) = unsafeSetOption' fun "iterative_solver" x
idasUnsafeSetOption fun (Idas.Jac_for_sens x) = unsafeSetOption' fun "jac_for_sens" x
idasUnsafeSetOption fun (Idas.Linear_solver x) = unsafeSetOption' fun "linear_solver" x
idasUnsafeSetOption fun (Idas.Lower_bandwidth x) = unsafeSetOption' fun "lower_bandwidth" x
idasUnsafeSetOption fun (Idas.Max_krylov x) = unsafeSetOption' fun "max_krylov" x
idasUnsafeSetOption fun (Idas.Max_multistep_order x) = unsafeSetOption' fun "max_multistep_order" x
idasUnsafeSetOption fun (Idas.Max_num_steps x) = unsafeSetOption' fun "max_num_steps" x
idasUnsafeSetOption fun (Idas.Max_step_size x) = unsafeSetOption' fun "max_step_size" x
idasUnsafeSetOption fun (Idas.Monitor x) = unsafeSetOption' fun "monitor" x
idasUnsafeSetOption fun (Idas.Name x) = unsafeSetOption' fun "name" x
idasUnsafeSetOption fun (Idas.Nrhs x) = unsafeSetOption' fun "nrhs" x
idasUnsafeSetOption fun (Idas.Number_of_adj_dir x) = unsafeSetOption' fun "number_of_adj_dir" x
idasUnsafeSetOption fun (Idas.Number_of_fwd_dir x) = unsafeSetOption' fun "number_of_fwd_dir" x
idasUnsafeSetOption fun (Idas.Numeric_hessian x) = unsafeSetOption' fun "numeric_hessian" x
idasUnsafeSetOption fun (Idas.Numeric_jacobian x) = unsafeSetOption' fun "numeric_jacobian" x
idasUnsafeSetOption fun (Idas.Pretype x) = unsafeSetOption' fun "pretype" x
idasUnsafeSetOption fun (Idas.Print_stats x) = unsafeSetOption' fun "print_stats" x
idasUnsafeSetOption fun (Idas.Quad_err_con x) = unsafeSetOption' fun "quad_err_con" x
idasUnsafeSetOption fun (Idas.Reltol x) = unsafeSetOption' fun "reltol" x
idasUnsafeSetOption fun (Idas.Sensitivity_method x) = unsafeSetOption' fun "sensitivity_method" x
idasUnsafeSetOption fun (Idas.Sparse x) = unsafeSetOption' fun "sparse" x
idasUnsafeSetOption fun (Idas.Steps_per_checkpoint x) = unsafeSetOption' fun "steps_per_checkpoint" x
idasUnsafeSetOption fun (Idas.Stop_at_end x) = unsafeSetOption' fun "stop_at_end" x
idasUnsafeSetOption fun (Idas.Store_jacobians x) = unsafeSetOption' fun "store_jacobians" x
idasUnsafeSetOption fun (Idas.Suppress_algebraic x) = unsafeSetOption' fun "suppress_algebraic" x
idasUnsafeSetOption fun (Idas.T0 x) = unsafeSetOption' fun "t0" x
idasUnsafeSetOption fun (Idas.Tf x) = unsafeSetOption' fun "tf" x
idasUnsafeSetOption fun (Idas.Upper_bandwidth x) = unsafeSetOption' fun "upper_bandwidth" x
idasUnsafeSetOption fun (Idas.Use_preconditioner x) = unsafeSetOption' fun "use_preconditioner" x
idasUnsafeSetOption fun (Idas.Verbose x) = unsafeSetOption' fun "verbose" x

cvodesUnsafeSetOption :: SXFunction -> Cvodes.CvodesOption -> IO ()
cvodesUnsafeSetOption fun (Cvodes.Abstol x) = unsafeSetOption' fun "abstol" x
cvodesUnsafeSetOption fun (Cvodes.Ad_mode x) = unsafeSetOption' fun "ad_mode" x
cvodesUnsafeSetOption fun (Cvodes.Asens_abstol x) = unsafeSetOption' fun "asens_abstol" x
cvodesUnsafeSetOption fun (Cvodes.Asens_iterative_solver x) = unsafeSetOption' fun "asens_iterative_solver" x
cvodesUnsafeSetOption fun (Cvodes.Asens_linear_solver x) = unsafeSetOption' fun "asens_linear_solver" x
cvodesUnsafeSetOption fun (Cvodes.Asens_lower_bandwidth x) = unsafeSetOption' fun "asens_lower_bandwidth" x
cvodesUnsafeSetOption fun (Cvodes.Asens_max_krylov x) = unsafeSetOption' fun "asens_max_krylov" x
cvodesUnsafeSetOption fun (Cvodes.Asens_pretype x) = unsafeSetOption' fun "asens_pretype" x
cvodesUnsafeSetOption fun (Cvodes.Asens_reltol x) = unsafeSetOption' fun "asens_reltol" x
cvodesUnsafeSetOption fun (Cvodes.Asens_upper_bandwidth x) = unsafeSetOption' fun "asens_upper_bandwidth" x
cvodesUnsafeSetOption fun (Cvodes.Disable_internal_warnings x) = unsafeSetOption' fun "disable_internal_warnings" x
cvodesUnsafeSetOption fun (Cvodes.Exact_jacobian x) = unsafeSetOption' fun "exact_jacobian" x
cvodesUnsafeSetOption fun (Cvodes.Finite_difference_fsens x) = unsafeSetOption' fun "finite_difference_fsens" x
cvodesUnsafeSetOption fun (Cvodes.Fsens_abstol x) = unsafeSetOption' fun "fsens_abstol" x
cvodesUnsafeSetOption fun (Cvodes.Fsens_all_at_once x) = unsafeSetOption' fun "fsens_all_at_once" x
cvodesUnsafeSetOption fun (Cvodes.Fsens_err_con x) = unsafeSetOption' fun "fsens_err_con" x
cvodesUnsafeSetOption fun (Cvodes.Fsens_reltol x) = unsafeSetOption' fun "fsens_reltol" x
cvodesUnsafeSetOption fun (Cvodes.Fsens_scaling_factors x) = unsafeSetOption' fun "fsens_scaling_factors" x
cvodesUnsafeSetOption fun (Cvodes.Fsens_sensitiviy_parameters x) = unsafeSetOption' fun "fsens_sensitiviy_parameters" x
cvodesUnsafeSetOption fun (Cvodes.Interpolation_type x) = unsafeSetOption' fun "interpolation_type" x
cvodesUnsafeSetOption fun (Cvodes.Iterative_solver x) = unsafeSetOption' fun "iterative_solver" x
cvodesUnsafeSetOption fun (Cvodes.Jac_for_sens x) = unsafeSetOption' fun "jac_for_sens" x
cvodesUnsafeSetOption fun (Cvodes.Linear_multistep_method x) = unsafeSetOption' fun "linear_multistep_method" x
cvodesUnsafeSetOption fun (Cvodes.Linear_solver x) = unsafeSetOption' fun "linear_solver" x
cvodesUnsafeSetOption fun (Cvodes.Lower_bandwidth x) = unsafeSetOption' fun "lower_bandwidth" x
cvodesUnsafeSetOption fun (Cvodes.Max_krylov x) = unsafeSetOption' fun "max_krylov" x
cvodesUnsafeSetOption fun (Cvodes.Max_multistep_order x) = unsafeSetOption' fun "max_multistep_order" x
cvodesUnsafeSetOption fun (Cvodes.Max_num_steps x) = unsafeSetOption' fun "max_num_steps" x
cvodesUnsafeSetOption fun (Cvodes.Monitor x) = unsafeSetOption' fun "monitor" x
cvodesUnsafeSetOption fun (Cvodes.Name x) = unsafeSetOption' fun "name" x
cvodesUnsafeSetOption fun (Cvodes.Nonlinear_solver_iteration x) = unsafeSetOption' fun "nonlinear_solver_iteration" x
cvodesUnsafeSetOption fun (Cvodes.Nrhs x) = unsafeSetOption' fun "nrhs" x
cvodesUnsafeSetOption fun (Cvodes.Number_of_adj_dir x) = unsafeSetOption' fun "number_of_adj_dir" x
cvodesUnsafeSetOption fun (Cvodes.Number_of_fwd_dir x) = unsafeSetOption' fun "number_of_fwd_dir" x
cvodesUnsafeSetOption fun (Cvodes.Numeric_hessian x) = unsafeSetOption' fun "numeric_hessian" x
cvodesUnsafeSetOption fun (Cvodes.Numeric_jacobian x) = unsafeSetOption' fun "numeric_jacobian" x
cvodesUnsafeSetOption fun (Cvodes.Pretype x) = unsafeSetOption' fun "pretype" x
cvodesUnsafeSetOption fun (Cvodes.Print_stats x) = unsafeSetOption' fun "print_stats" x
cvodesUnsafeSetOption fun (Cvodes.Quad_err_con x) = unsafeSetOption' fun "quad_err_con" x
cvodesUnsafeSetOption fun (Cvodes.Reltol x) = unsafeSetOption' fun "reltol" x
cvodesUnsafeSetOption fun (Cvodes.Sensitivity_method x) = unsafeSetOption' fun "sensitivity_method" x
cvodesUnsafeSetOption fun (Cvodes.Sparse x) = unsafeSetOption' fun "sparse" x
cvodesUnsafeSetOption fun (Cvodes.Steps_per_checkpoint x) = unsafeSetOption' fun "steps_per_checkpoint" x
cvodesUnsafeSetOption fun (Cvodes.Stop_at_end x) = unsafeSetOption' fun "stop_at_end" x
cvodesUnsafeSetOption fun (Cvodes.Store_jacobians x) = unsafeSetOption' fun "store_jacobians" x
cvodesUnsafeSetOption fun (Cvodes.T0 x) = unsafeSetOption' fun "t0" x
cvodesUnsafeSetOption fun (Cvodes.Tf x) = unsafeSetOption' fun "tf" x
cvodesUnsafeSetOption fun (Cvodes.Upper_bandwidth x) = unsafeSetOption' fun "upper_bandwidth" x
cvodesUnsafeSetOption fun (Cvodes.Use_preconditioner x) = unsafeSetOption' fun "use_preconditioner" x
cvodesUnsafeSetOption fun (Cvodes.Verbose x) = unsafeSetOption' fun "verbose" x
