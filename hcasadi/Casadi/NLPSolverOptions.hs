{-# OPTIONS_GHC -Wall #-}

module Casadi.NLPSolverOptions ( NLPSolverOption(..)
                               ) where

data NLPSolverOption = 
    Accept_after_max_steps Int -- ^ (-1) Accept a trial point after maximal this number of steps. (see IPOPT documentation)
  | Accept_every_trial_step String -- ^ (no) Always accept the first trial step. (see IPOPT documentation)
  | Acceptable_compl_inf_tol Double -- ^ (0.01) "Acceptance" threshold for the complementarity conditions. (see IPOPT documentation)
  | Acceptable_constr_viol_tol Double -- ^ (0.01) "Acceptance" threshold for the constraint violation. (see IPOPT documentation)
  | Acceptable_dual_inf_tol Double -- ^ (10000000000.0) "Acceptance" threshold for the dual infeasibility. (see IPOPT documentation)
  | Acceptable_iter Int -- ^ (15) Number of "acceptable" iterates before triggering termination. (see IPOPT documentation)
  | Acceptable_obj_change_tol Double -- ^ (1e+20) "Acceptance" stopping criterion based on objective function change. (see IPOPT documentation)
  | Acceptable_tol Double -- ^ (1e-06) "Acceptable" convergence tolerance (relative). (see IPOPT documentation)
  | Ad_mode String -- ^ (automatic) How to calculate the Jacobians: "forward" (only forward mode) "reverse" (only adjoint mode) or "automatic" (a heuristic decides which is more appropriate)
  | Adaptive_mu_globalization String -- ^ (obj-constr-filter) Globalization strategy for the adaptive mu selection mode. (see IPOPT documentation)
  | Adaptive_mu_kkt_norm_type String -- ^ (2-norm-squared) Norm used for the KKT error in the adaptive mu globalization strategies. (see IPOPT documentation)
  | Adaptive_mu_kkterror_red_fact Double -- ^ (0.9999) Sufficient decrease factor for "kkt-error" globalization strategy. (see IPOPT documentation)
  | Adaptive_mu_kkterror_red_iters Int -- ^ (4) Maximum number of iterations requiring sufficient progress. (see IPOPT documentation)
  | Adaptive_mu_monotone_init_factor Double -- ^ (0.8) Determines the initial value of the barrier parameter when switching to the monotone mode. (see IPOPT documentation)
  | Adaptive_mu_restore_previous_iterate String -- ^ (no) Indicates if the previous iterate should be restored if the monotone mode is entered. (see IPOPT documentation)
  | Adaptive_mu_safeguard_factor Double -- ^ (0.0)  (see IPOPT documentation)
  | Alpha_for_y String -- ^ (primal) Method to determine the step size for constraint multipliers. (see IPOPT documentation)
  | Alpha_for_y_tol Double -- ^ (10.0) Tolerance for switching to full equality multiplier steps. (see IPOPT documentation)
  | Alpha_min_frac Double -- ^ (0.05) Safety factor for the minimal step size (before switching to restoration phase). (see IPOPT documentation)
  | Alpha_red_factor Double -- ^ (0.5) Fractional reduction of the trial step size in the backtracking line search. (see IPOPT documentation)
  | Barrier_tol_factor Double -- ^ (10.0) Factor for mu in barrier stop test. (see IPOPT documentation)
  | Bound_frac Double -- ^ (0.01) Desired minimum relative distance from the initial point to bound. (see IPOPT documentation)
  | Bound_mult_init_method String -- ^ (constant) Initialization method for bound multipliers (see IPOPT documentation)
  | Bound_mult_init_val Double -- ^ (1.0) Initial value for the bound multipliers. (see IPOPT documentation)
  | Bound_mult_reset_threshold Double -- ^ (1000.0) Threshold for resetting bound multipliers after the restoration phase. (see IPOPT documentation)
  | Bound_push Double -- ^ (0.01) Desired minimum absolute distance from the initial point to bound. (see IPOPT documentation)
  | Bound_relax_factor Double -- ^ (1e-08) Factor for initial relaxation of the bounds. (see IPOPT documentation)
  | Check_derivatives_for_naninf String -- ^ (no) Indicates whether it is desired to check for Nan/Inf in derivative matrices (see IPOPT documentation)
  | Chi_cup Double -- ^ (1.5) LIFENG WRITES THIS. (see IPOPT documentation)
  | Chi_hat Double -- ^ (2.0) LIFENG WRITES THIS. (see IPOPT documentation)
  | Chi_tilde Double -- ^ (5.0) LIFENG WRITES THIS. (see IPOPT documentation)
  | Compl_inf_tol Double -- ^ (0.0001) Desired threshold for the complementarity conditions. (see IPOPT documentation)
  | Constr_mult_init_max Double -- ^ (1000.0) Maximum allowed least-square guess of constraint multipliers. (see IPOPT documentation)
  | Constr_mult_reset_threshold Double -- ^ (0.0) Threshold for resetting equality and inequality multipliers after restoration phase. (see IPOPT documentation)
  | Constr_viol_tol Double -- ^ (0.0001) Desired threshold for the constraint violation. (see IPOPT documentation)
  | Constraint_violation_norm_type String -- ^ (1-norm) Norm to be used for the constraint violation in the line search. (see IPOPT documentation)
  | Corrector_compl_avrg_red_fact Double -- ^ (1.0) Complementarity tolerance factor for accepting corrector step (unsupported!). (see IPOPT documentation)
  | Corrector_type String -- ^ (none) The type of corrector steps that should be taken (unsupported!). (see IPOPT documentation)
  | Delta Double -- ^ (1.0) Multiplier for constraint violation in the switching rule. (see IPOPT documentation)
  | Delta_y_max Double -- ^ (1e+12) a parameter used to check if the fast direction can be used asthe line search direction (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Dependency_detection_with_rhs String -- ^ (no) Indicates if the right hand sides of the constraints should be considered during dependency detection (see IPOPT documentation)
  | Dependency_detector String -- ^ (none) Indicates which linear solver should be used to detect linearly dependent equality constraints. (see IPOPT documentation)
  | Derivative_test String -- ^ (none) Enable derivative checker (see IPOPT documentation)
  | Derivative_test_first_index Int -- ^ (-2) Index of first quantity to be checked by derivative checker (see IPOPT documentation)
  | Derivative_test_perturbation Double -- ^ (1e-08) Size of the finite difference perturbation in derivative test. (see IPOPT documentation)
  | Derivative_test_print_all String -- ^ (no) Indicates whether information for all estimated derivatives should be printed. (see IPOPT documentation)
  | Derivative_test_tol Double -- ^ (0.0001) Threshold for indicating wrong derivative. (see IPOPT documentation)
  | Diverging_iterates_tol Double -- ^ (1e+20) Threshold for maximal value of primal iterates. (see IPOPT documentation)
  | Dual_inf_tol Double -- ^ (1.0) Desired threshold for the dual infeasibility. (see IPOPT documentation)
  | Epsilon_c Double -- ^ (0.01) LIFENG WRITES THIS. (see IPOPT documentation)
  | Eta_min Double -- ^ (10.0) LIFENG WRITES THIS. (see IPOPT documentation)
  | Eta_penalty Double -- ^ (1e-08) Relaxation factor in the Armijo condition for the penalty function. (see IPOPT documentation)
  | Eta_phi Double -- ^ (1e-08) Relaxation factor in the Armijo condition. (see IPOPT documentation)
  | Evaluate_orig_obj_at_resto_trial String -- ^ (yes) Determines if the original objective function should be evaluated at restoration phase trial points. (see IPOPT documentation)
  | Expand_f Bool -- ^ (False) Expand the objective function in terms of scalar operations, i.e. MX->SX
  | Expand_g Bool -- ^ (False) Expand the constraint function in terms of scalar operations, i.e. MX->SX
  | Expect_infeasible_problem String -- ^ (no) Enable heuristics to quickly detect an infeasible problem. (see IPOPT documentation)
  | Expect_infeasible_problem_ctol Double -- ^ (0.001) Threshold for disabling "expect_infeasible_problem" option. (see IPOPT documentation)
  | Expect_infeasible_problem_ytol Double -- ^ (100000000.0) Multiplier threshold for activating "expect_infeasible_problem" option. (see IPOPT documentation)
  | Fast_des_fact Double -- ^ (0.1) a parameter used to check if the fast direction can be used asthe line search direction (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Fast_step_computation String -- ^ (no) Indicates if the linear system should be solved quickly. (see IPOPT documentation)
  | File_print_level Int -- ^ (5) Verbosity level for output file. (see IPOPT documentation)
  | Filter_margin_fact Double -- ^ (1e-05) Factor determining width of margin for obj-constr-filter adaptive globalization strategy. (see IPOPT documentation)
  | Filter_max_margin Double -- ^ (1.0) Maximum width of margin in obj-constr-filter adaptive globalization strategy. (see IPOPT documentation)
  | Filter_reset_trigger Int -- ^ (5) Number of iterations that trigger the filter reset. (see IPOPT documentation)
  | Findiff_perturbation Double -- ^ (1e-07) Size of the finite difference perturbation for derivative approximation. (see IPOPT documentation)
  | First_hessian_perturbation Double -- ^ (0.0001) Size of first x-s perturbation tried. (see IPOPT documentation)
  | Fixed_mu_oracle String -- ^ (average_compl) Oracle for the barrier parameter when switching to fixed mode. (see IPOPT documentation)
  | Fixed_variable_treatment String -- ^ (make_parameter) Determines how fixed variables should be handled. (see IPOPT documentation)
  | Gamma_hat Double -- ^ (0.04) LIFENG WRITES THIS. (see IPOPT documentation)
  | Gamma_phi Double -- ^ (1e-08) Relaxation factor in the filter margin for the barrier function. (see IPOPT documentation)
  | Gamma_theta Double -- ^ (1e-05) Relaxation factor in the filter margin for the constraint violation. (see IPOPT documentation)
  | Gamma_tilde Double -- ^ (4.0) LIFENG WRITES THIS. (see IPOPT documentation)
  | Gauss_newton Bool -- ^ (False) Use Gauss Newton Hessian approximation
  | Generate_hessian Bool -- ^ (False) Generate an exact Hessian of the Lagrangian if not supplied
  | Generate_jacobian Bool -- ^ (True) Generate an exact Jacobian of the constraints if not supplied
  | Hessian_approximation String -- ^ (exact) Indicates what Hessian information is to be used. (see IPOPT documentation)
  | Hessian_approximation_space String -- ^ (nonlinear-variables) Indicates in which subspace the Hessian information is to be approximated. (see IPOPT documentation)
  | Hessian_constant String -- ^ (no) Indicates whether the problem is a quadratic problem (see IPOPT documentation)
  | Honor_original_bounds String -- ^ (yes) Indicates whether final points should be projected into original bounds. (see IPOPT documentation)
  | Ignore_check_vec Bool -- ^ (False) If set to true, the input shape of F will not be checked.
  | Inf_pr_output String -- ^ (original) Determines what value is printed in the "inf_pr" output column. (see IPOPT documentation)
  | Iteration_callback_ignore_errors Bool -- ^ (False) If set to true, errors thrown by iteration_callback will be ignored.
  | Iteration_callback_step Int -- ^ (1) Only call the callback function every few iterations.
  | Jac_c_constant String -- ^ (no) Indicates whether all equality constraints are linear (see IPOPT documentation)
  | Jac_d_constant String -- ^ (no) Indicates whether all inequality constraints are linear (see IPOPT documentation)
  | Jac_for_sens Bool -- ^ (False) Create the a Jacobian function and use this to calculate forward sensitivities
  | Jacobian_approximation String -- ^ (exact) Specifies technique to compute constraint Jacobian (see IPOPT documentation)
  | Jacobian_regularization_exponent Double -- ^ (0.25) Exponent for mu in the regularization for rank-deficient constraint Jacobians. (see IPOPT documentation)
  | Jacobian_regularization_value Double -- ^ (1e-08) Size of the regularization for rank-deficient constraint Jacobians. (see IPOPT documentation)
  | Kappa_d Double -- ^ (1e-05) Weight for linear damping term (to handle one-sided bounds). (see IPOPT documentation)
  | Kappa_sigma Double -- ^ (10000000000.0) Factor limiting the deviation of dual variables from primal estimates. (see IPOPT documentation)
  | Kappa_soc Double -- ^ (0.99) Factor in the sufficient reduction rule for second order correction. (see IPOPT documentation)
  | Kappa_x_dis Double -- ^ (100.0) a parameter used to check if the fast direction can be used asthe line search direction (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Kappa_y_dis Double -- ^ (10000.0) a parameter used to check if the fast direction can be used asthe line search direction (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Least_square_init_duals String -- ^ (no) Least square initialization of all dual variables (see IPOPT documentation)
  | Least_square_init_primal String -- ^ (no) Least square initialization of the primal variables (see IPOPT documentation)
  | Limited_memory_aug_solver String -- ^ (sherman-morrison) Strategy for solving the augmented system for low-rank Hessian. (see IPOPT documentation)
  | Limited_memory_init_val Double -- ^ (1.0) Value for B0 in low-rank update. (see IPOPT documentation)
  | Limited_memory_init_val_max Double -- ^ (100000000.0) Upper bound on value for B0 in low-rank update. (see IPOPT documentation)
  | Limited_memory_init_val_min Double -- ^ (1e-08) Lower bound on value for B0 in low-rank update. (see IPOPT documentation)
  | Limited_memory_initialization String -- ^ (scalar1) Initialization strategy for the limited memory quasi-Newton approximation. (see IPOPT documentation)
  | Limited_memory_max_history Int -- ^ (6) Maximum size of the history for the limited quasi-Newton Hessian approximation. (see IPOPT documentation)
  | Limited_memory_max_skipping Int -- ^ (2) Threshold for successive iterations where update is skipped. (see IPOPT documentation)
  | Limited_memory_special_for_resto String -- ^ (no) Determines if the quasi-Newton updates should be special during the restoration phase. (see IPOPT documentation)
  | Limited_memory_update_type String -- ^ (bfgs) Quasi-Newton update formula for the limited memory approximation. (see IPOPT documentation)
  | Line_search_method String -- ^ (filter) Globalization method used in backtracking line search (see IPOPT documentation)
  | Linear_scaling_on_demand String -- ^ (yes) Flag indicating that linear scaling is only done if it seems required. (see IPOPT documentation)
  | Linear_solver String -- ^ (ma27) Linear solver used for step computations. (see IPOPT documentation)
  | Linear_system_scaling String -- ^ (mc19) Method for scaling the linear system. (see IPOPT documentation)
  | Ma27_ignore_singularity String -- ^ (no) Enables MA27's ability to solve a linear system even if the matrix is singular. (see IPOPT documentation)
  | Ma27_la_init_factor Double -- ^ (5.0) Real workspace memory for MA27. (see IPOPT documentation)
  | Ma27_liw_init_factor Double -- ^ (5.0) Integer workspace memory for MA27. (see IPOPT documentation)
  | Ma27_meminc_factor Double -- ^ (10.0) Increment factor for workspace size for MA27. (see IPOPT documentation)
  | Ma27_pivtol Double -- ^ (1e-08) Pivot tolerance for the linear solver MA27. (see IPOPT documentation)
  | Ma27_pivtolmax Double -- ^ (0.0001) Maximum pivot tolerance for the linear solver MA27. (see IPOPT documentation)
  | Ma27_skip_inertia_check String -- ^ (no) Always pretend inertia is correct. (see IPOPT documentation)
  | Ma28_pivtol Double -- ^ (0.01) Pivot tolerance for linear solver MA28. (see IPOPT documentation)
  | Ma57_automatic_scaling String -- ^ (yes) Controls MA57 automatic scaling (see IPOPT documentation)
  | Ma57_block_size Int -- ^ (16) Controls block size used by Level 3 BLAS in MA57BD (see IPOPT documentation)
  | Ma57_node_amalgamation Int -- ^ (16) Node amalgamation parameter (see IPOPT documentation)
  | Ma57_pivot_order Int -- ^ (5) Controls pivot order in MA57 (see IPOPT documentation)
  | Ma57_pivtol Double -- ^ (1e-08) Pivot tolerance for the linear solver MA57. (see IPOPT documentation)
  | Ma57_pivtolmax Double -- ^ (0.0001) Maximum pivot tolerance for the linear solver MA57. (see IPOPT documentation)
  | Ma57_pre_alloc Double -- ^ (1.05) Safety factor for work space memory allocation for the linear solver MA57. (see IPOPT documentation)
  | Ma57_small_pivot_flag Int -- ^ (0) If set to 1, then when small entries defined by CNTL(2) are detected they are removed and the corresponding pivots placed at the end of the factorization.  This can be particularly efficient if the matrix is highly rank deficient. (see IPOPT documentation)
  | Ma86_nemin Int -- ^ (32) Node Amalgamation parameter (see IPOPT documentation)
  | Ma86_order String -- ^ (auto) Controls type of ordering used by HSL_MA86 (see IPOPT documentation)
  | Ma86_print_level Int -- ^ (-1) Debug printing level for the linear solver MA86 (see IPOPT documentation)
  | Ma86_scaling String -- ^ (mc64) Controls scaling of matrix (see IPOPT documentation)
  | Ma86_small Double -- ^ (1e-20) Zero Pivot Threshold (see IPOPT documentation)
  | Ma86_static Double -- ^ (0.0) Static Pivoting Threshold (see IPOPT documentation)
  | Ma86_u Double -- ^ (1e-08) Pivoting Threshold (see IPOPT documentation)
  | Ma86_umax Double -- ^ (0.0001) Maximum Pivoting Threshold (see IPOPT documentation)
  | Magic_steps String -- ^ (no) Enables magic steps. (see IPOPT documentation)
  | Max_cpu_time Double -- ^ (1000000.0) Maximum number of CPU seconds. (see IPOPT documentation)
  | Max_filter_resets Int -- ^ (5) Maximal allowed number of filter resets (see IPOPT documentation)
  | Max_hessian_perturbation Double -- ^ (1e+20) Maximum value of regularization parameter for handling negative curvature. (see IPOPT documentation)
  | Max_iter Int -- ^ (3000) Maximum number of iterations. (see IPOPT documentation)
  | Max_refinement_steps Int -- ^ (10) Maximum number of iterative refinement steps per linear system solve. (see IPOPT documentation)
  | Max_resto_iter Int -- ^ (3000000) Maximum number of successive iterations in restoration phase. (see IPOPT documentation)
  | Max_soc Int -- ^ (4) Maximum number of second order correction trial steps at each iteration. (see IPOPT documentation)
  | Max_soft_resto_iters Int -- ^ (10) Maximum number of iterations performed successively in soft restoration phase. (see IPOPT documentation)
  | Mehrotra_algorithm String -- ^ (no) Indicates if we want to do Mehrotra's algorithm. (see IPOPT documentation)
  | Min_alpha_primal Double -- ^ (1e-13) LIFENG WRITES THIS. (see IPOPT documentation)
  | Min_hessian_perturbation Double -- ^ (1e-20) Smallest perturbation of the Hessian block. (see IPOPT documentation)
  | Min_refinement_steps Int -- ^ (1) Minimum number of iterative refinement steps per linear system solve. (see IPOPT documentation)
  | Monitor [String] -- ^ (???) Monitors to be activated
  | Mu_allow_fast_monotone_decrease String -- ^ (yes) Allow skipping of barrier problem if barrier test is already met. (see IPOPT documentation)
  | Mu_init Double -- ^ (0.1) Initial value for the barrier parameter. (see IPOPT documentation)
  | Mu_linear_decrease_factor Double -- ^ (0.2) Determines linear decrease rate of barrier parameter. (see IPOPT documentation)
  | Mu_max Double -- ^ (100000.0) Maximum value for barrier parameter. (see IPOPT documentation)
  | Mu_max_fact Double -- ^ (1000.0) Factor for initialization of maximum value for barrier parameter. (see IPOPT documentation)
  | Mu_min Double -- ^ (1e-11) Minimum value for barrier parameter. (see IPOPT documentation)
  | Mu_oracle String -- ^ (quality-function) Oracle for a new barrier parameter in the adaptive strategy. (see IPOPT documentation)
  | Mu_strategy String -- ^ (monotone) Update strategy for barrier parameter. (see IPOPT documentation)
  | Mu_superlinear_decrease_power Double -- ^ (1.5) Determines superlinear decrease rate of barrier parameter. (see IPOPT documentation)
  | Mu_target Double -- ^ (0.0) Desired value of complementarity. (see IPOPT documentation)
  | Mult_diverg_feasibility_tol Double -- ^ (1e-07) tolerance for deciding if the multipliers are diverging (see IPOPT documentation)
  | Mult_diverg_y_tol Double -- ^ (100000000.0) tolerance for deciding if the multipliers are diverging (see IPOPT documentation)
  | Mumps_dep_tol Double -- ^ (-1.0) Pivot threshold for detection of linearly dependent constraints in MUMPS. (see IPOPT documentation)
  | Mumps_mem_percent Int -- ^ (1000) Percentage increase in the estimated working space for MUMPS. (see IPOPT documentation)
  | Mumps_permuting_scaling Int -- ^ (7) Controls permuting and scaling in MUMPS (see IPOPT documentation)
  | Mumps_pivot_order Int -- ^ (7) Controls pivot order in MUMPS (see IPOPT documentation)
  | Mumps_pivtol Double -- ^ (1e-06) Pivot tolerance for the linear solver MUMPS. (see IPOPT documentation)
  | Mumps_pivtolmax Double -- ^ (0.1) Maximum pivot tolerance for the linear solver MUMPS. (see IPOPT documentation)
  | Mumps_scaling Int -- ^ (77) Controls scaling in MUMPS (see IPOPT documentation)
  | Name String -- ^ (unnamed_shared_object) n/a
  | Neg_curv_test_tol Double -- ^ (0.0) Tolerance for heuristic to ignore wrong inertia. (see IPOPT documentation)
  | Never_use_fact_cgpen_direction String -- ^ (no) Toggle to switch off the fast Chen-Goldfarb direction (see IPOPT documentation)
  | Never_use_piecewise_penalty_ls String -- ^ (no) Toggle to switch off the piecewise penalty method (see IPOPT documentation)
  | Nlp_lower_bound_inf Double -- ^ (-1e+19) any bound less or equal this value will be considered -inf (i.e. not lower bounded). (see IPOPT documentation)
  | Nlp_scaling_constr_target_gradient Double -- ^ (0.0) Target value for constraint function gradient size. (see IPOPT documentation)
  | Nlp_scaling_max_gradient Double -- ^ (100.0) Maximum gradient after NLP scaling. (see IPOPT documentation)
  | Nlp_scaling_method String -- ^ (gradient-based) Select the technique used for scaling the NLP. (see IPOPT documentation)
  | Nlp_scaling_min_value Double -- ^ (1e-08) Minimum value of gradient-based scaling values. (see IPOPT documentation)
  | Nlp_scaling_obj_target_gradient Double -- ^ (0.0) Target value for objective function gradient size. (see IPOPT documentation)
  | Nlp_upper_bound_inf Double -- ^ (1e+19) any bound greater or this value will be considered +inf (i.e. not upper bounded). (see IPOPT documentation)
  | Nu_inc Double -- ^ (0.0001) Increment of the penalty parameter. (see IPOPT documentation)
  | Nu_init Double -- ^ (1e-06) Initial value of the penalty parameter. (see IPOPT documentation)
  | Num_linear_variables Int -- ^ (0) Number of linear variables (see IPOPT documentation)
  | Number_of_adj_dir Int -- ^ (1) number of adjoint derivatives to be calculated simultanously
  | Number_of_fwd_dir Int -- ^ (1) number of forward derivatives to be calculated simultanously
  | Numeric_hessian Bool -- ^ (False) Calculate Hessians numerically (using directional derivatives) rather than with the built-in method
  | Numeric_jacobian Bool -- ^ (False) Calculate Jacobians numerically (using directional derivatives) rather than with the built-in method
  | Obj_max_inc Double -- ^ (5.0) Determines the upper bound on the acceptable increase of barrier objective function. (see IPOPT documentation)
  | Obj_scaling_factor Double -- ^ (1.0) Scaling factor for the objective function. (see IPOPT documentation)
  | Option_file_name String -- ^ () File name of options file (to overwrite default). (see IPOPT documentation)
  | Output_file String -- ^ () File name of desired output file (leave unset for no file output). (see IPOPT documentation)
  | Parametric Bool -- ^ (False) Expect F, G, H, J to have an additional input argument appended at the end, denoting fixed parameters.
  | Pardiso_iter_coarse_size Int -- ^ (5000) Maximum Size of Coarse Grid Matrix (see IPOPT documentation)
  | Pardiso_iter_dropping_factor Double -- ^ (0.5) dropping value for incomplete factor (see IPOPT documentation)
  | Pardiso_iter_dropping_schur Double -- ^ (0.1) dropping value for sparsify schur complement factor (see IPOPT documentation)
  | Pardiso_iter_inverse_norm_factor Double -- ^ (5000000.0)  (see IPOPT documentation)
  | Pardiso_iter_max_levels Int -- ^ (10) Maximum Size of Grid Levels (see IPOPT documentation)
  | Pardiso_iter_max_row_fill Int -- ^ (10000000) max fill for each row (see IPOPT documentation)
  | Pardiso_iter_relative_tol Double -- ^ (1e-06) Relative Residual Convergence (see IPOPT documentation)
  | Pardiso_iterative String -- ^ (no) Switch on iterative solver in Pardiso library (see IPOPT documentation)
  | Pardiso_matching_strategy String -- ^ (complete+2x2) Matching strategy to be used by Pardiso (see IPOPT documentation)
  | Pardiso_max_droptol_corrections Int -- ^ (4) Maximal number of decreases of drop tolerance during one solve. (see IPOPT documentation)
  | Pardiso_max_iter Int -- ^ (500) Maximum number of Krylov-Subspace Iteration (see IPOPT documentation)
  | Pardiso_msglvl Int -- ^ (0) Pardiso message level (see IPOPT documentation)
  | Pardiso_out_of_core_power Int -- ^ (0) Enables out-of-core variant of Pardiso (see IPOPT documentation)
  | Pardiso_redo_symbolic_fact_only_if_inertia_wrong String -- ^ (no) Toggle for handling case when elements were perturbed by Pardiso. (see IPOPT documentation)
  | Pardiso_repeated_perturbation_means_singular String -- ^ (no) Interpretation of perturbed elements. (see IPOPT documentation)
  | Pardiso_skip_inertia_check String -- ^ (no) Always pretend inertia is correct. (see IPOPT documentation)
  | Pass_nonlinear_variables Bool -- ^ (True) n/a
  | Pen_des_fact Double -- ^ (0.2) a parameter used in penalty parameter computation (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Pen_init_fac Double -- ^ (50.0) a parameter used to choose initial penalty parameterswhen the regularized Newton method is used. (see IPOPT documentation)
  | Pen_theta_max_fact Double -- ^ (10000.0) Determines upper bound for constraint violation in the filter. (see IPOPT documentation)
  | Penalty_init_max Double -- ^ (100000.0) Maximal value for the intial penalty parameter (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Penalty_init_min Double -- ^ (1.0) Minimal value for the intial penalty parameter for line search(for Chen-Goldfarb line search). (see IPOPT documentation)
  | Penalty_max Double -- ^ (1e+30) Maximal value for the penalty parameter (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Penalty_update_compl_tol Double -- ^ (10.0) LIFENG WRITES THIS. (see IPOPT documentation)
  | Penalty_update_infeasibility_tol Double -- ^ (1e-09) Threshold for infeasibility in penalty parameter update test. (see IPOPT documentation)
  | Perturb_always_cd String -- ^ (no) Active permanent perturbation of constraint linearization. (see IPOPT documentation)
  | Perturb_dec_fact Double -- ^ (0.333333333333) Decrease factor for x-s perturbation. (see IPOPT documentation)
  | Perturb_inc_fact Double -- ^ (8.0) Increase factor for x-s perturbation. (see IPOPT documentation)
  | Perturb_inc_fact_first Double -- ^ (100.0) Increase factor for x-s perturbation for very first perturbation. (see IPOPT documentation)
  | Piecewisepenalty_gamma_infeasi Double -- ^ (1e-13) LIFENG WRITES THIS. (see IPOPT documentation)
  | Piecewisepenalty_gamma_obj Double -- ^ (1e-13) LIFENG WRITES THIS. (see IPOPT documentation)
  | Point_perturbation_radius Double -- ^ (10.0) Maximal perturbation of an evaluation point. (see IPOPT documentation)
  | Print_info_string String -- ^ (no) Enables printing of additional info string at end of iteration output. (see IPOPT documentation)
  | Print_level Int -- ^ (5) Output verbosity level. (see IPOPT documentation)
  | Print_options_documentation String -- ^ (no) Switch to print all algorithmic options. (see IPOPT documentation)
  | Print_options_latex_mode String -- ^ (no) Undocumented (see IPOPT documentation)
  | Print_time Bool -- ^ (True) print information about execution time
  | Print_timing_statistics String -- ^ (no) Switch to print timing statistics. (see IPOPT documentation)
  | Print_user_options String -- ^ (no) Print all options set by the user. (see IPOPT documentation)
  | Quality_function_balancing_term String -- ^ (none) The balancing term included in the quality function for centrality. (see IPOPT documentation)
  | Quality_function_centrality String -- ^ (none) The penalty term for centrality that is included in quality function. (see IPOPT documentation)
  | Quality_function_max_section_steps Int -- ^ (8) Maximum number of search steps during direct search procedure determining the optimal centering parameter. (see IPOPT documentation)
  | Quality_function_norm_type String -- ^ (2-norm-squared) Norm used for components of the quality function. (see IPOPT documentation)
  | Quality_function_section_qf_tol Double -- ^ (0.0) Tolerance for the golden section search procedure determining the optimal centering parameter (in the function value space). (see IPOPT documentation)
  | Quality_function_section_sigma_tol Double -- ^ (0.01) Tolerance for the section search procedure determining the optimal centering parameter (in sigma space). (see IPOPT documentation)
  | Recalc_y String -- ^ (no) Tells the algorithm to recalculate the equality and inequality multipliers as least square estimates. (see IPOPT documentation)
  | Recalc_y_feas_tol Double -- ^ (1e-06) Feasibility threshold for recomputation of multipliers. (see IPOPT documentation)
  | Replace_bounds String -- ^ (no) Indicates if all variable bounds should be replaced by inequality constraints (see IPOPT documentation)
  | Required_infeasibility_reduction Double -- ^ (0.9) Required reduction of infeasibility before leaving restoration phase. (see IPOPT documentation)
  | Residual_improvement_factor Double -- ^ (0.999999999) Minimal required reduction of residual test ratio in iterative refinement. (see IPOPT documentation)
  | Residual_ratio_max Double -- ^ (1e-10) Iterative refinement tolerance (see IPOPT documentation)
  | Residual_ratio_singular Double -- ^ (1e-05) Threshold for declaring linear system singular after failed iterative refinement. (see IPOPT documentation)
  | Resto_failure_feasibility_threshold Double -- ^ (0.0) Threshold for primal infeasibility to declare failure of restoration phase. (see IPOPT documentation)
  | Resto_penalty_parameter Double -- ^ (1000.0) Penalty parameter in the restoration phase objective function. (see IPOPT documentation)
  | Resto_proximity_weight Double -- ^ (1.0) Weighting factor for the proximity term in restoration phase objective. (see IPOPT documentation)
  | Rho Double -- ^ (0.1) Value in penalty parameter update formula. (see IPOPT documentation)
  | S_max Double -- ^ (100.0) Scaling threshold for the NLP error. (see IPOPT documentation)
  | S_phi Double -- ^ (2.3) Exponent for linear barrier function model in the switching rule. (see IPOPT documentation)
  | S_theta Double -- ^ (1.1) Exponent for current constraint violation in the switching rule. (see IPOPT documentation)
  | Sb String -- ^ (no)  (see IPOPT documentation)
  | Sigma_max Double -- ^ (100.0) Maximum value of the centering parameter. (see IPOPT documentation)
  | Sigma_min Double -- ^ (1e-06) Minimum value of the centering parameter. (see IPOPT documentation)
  | Skip_corr_if_neg_curv String -- ^ (yes) Skip the corrector step in negative curvature iteration (unsupported!). (see IPOPT documentation)
  | Skip_corr_in_monotone_mode String -- ^ (yes) Skip the corrector step during monotone barrier parameter mode (unsupported!). (see IPOPT documentation)
  | Skip_finalize_solution_call String -- ^ (no) Indicates if call to NLP::FinalizeSolution after optimization should be suppressed (see IPOPT documentation)
  | Slack_bound_frac Double -- ^ (0.01) Desired minimum relative distance from the initial slack to bound. (see IPOPT documentation)
  | Slack_bound_push Double -- ^ (0.01) Desired minimum absolute distance from the initial slack to bound. (see IPOPT documentation)
  | Slack_move Double -- ^ (1.81898940355e-12) Correction size for very small slacks. (see IPOPT documentation)
  | Soft_resto_pderror_reduction_factor Double -- ^ (0.9999) Required reduction in primal-dual error in the soft restoration phase. (see IPOPT documentation)
  | Sparse Bool -- ^ (True) function is sparse
  | Start_with_resto String -- ^ (no) Tells algorithm to switch to restoration phase in first iteration. (see IPOPT documentation)
  | Store_jacobians Bool -- ^ (False) keep references to generated Jacobians in order to avoid generating identical Jacobians multiple times
  | Suppress_all_output String -- ^ (no) Undocumented (see IPOPT documentation)
  | Tau_min Double -- ^ (0.99) Lower bound on fraction-to-the-boundary parameter tau. (see IPOPT documentation)
  | Theta_max_fact Double -- ^ (10000.0) Determines upper bound for constraint violation in the filter. (see IPOPT documentation)
  | Theta_min Double -- ^ (1e-06) LIFENG WRITES THIS. (see IPOPT documentation)
  | Theta_min_fact Double -- ^ (0.0001) Determines constraint violation threshold in the switching rule. (see IPOPT documentation)
  | Tiny_step_tol Double -- ^ (2.22044604925e-15) Tolerance for detecting numerically insignificant steps. (see IPOPT documentation)
  | Tiny_step_y_tol Double -- ^ (0.01) Tolerance for quitting because of numerically insignificant steps. (see IPOPT documentation)
  | Tol Double -- ^ (1e-08) Desired convergence tolerance (relative). (see IPOPT documentation)
  | Vartheta Double -- ^ (0.5) a parameter used to check if the fast direction can be used asthe line search direction (for Chen-Goldfarb line search). (see IPOPT documentation)
  | Verbose Bool -- ^ (False) verbose evaluation -- for debugging
  | Warm_start_bound_frac Double -- ^ (0.001) same as bound_frac for the regular initializer. (see IPOPT documentation)
  | Warm_start_bound_push Double -- ^ (0.001) same as bound_push for the regular initializer. (see IPOPT documentation)
  | Warm_start_entire_iterate String -- ^ (no) Tells algorithm whether to use the GetWarmStartIterate method in the NLP. (see IPOPT documentation)
  | Warm_start_init_point String -- ^ (no) Warm-start for initial point (see IPOPT documentation)
  | Warm_start_mult_bound_push Double -- ^ (0.001) same as mult_bound_push for the regular initializer. (see IPOPT documentation)
  | Warm_start_mult_init_max Double -- ^ (1000000.0) Maximum initial value for the equality multipliers. (see IPOPT documentation)
  | Warm_start_same_structure String -- ^ (no) Indicates whether a problem with a structure identical to the previous one is to be solved. (see IPOPT documentation)
  | Warm_start_slack_bound_frac Double -- ^ (0.001) same as slack_bound_frac for the regular initializer. (see IPOPT documentation)
  | Warm_start_slack_bound_push Double -- ^ (0.001) same as slack_bound_push for the regular initializer. (see IPOPT documentation)
  | Warm_start_target_mu Double -- ^ (0.0) Unsupported! (see IPOPT documentation)
  | Warn_initial_bounds Bool -- ^ (False) Warn if the initial guess does not satisfy LBX and UBX
  | Watchdog_shortened_iter_trigger Int -- ^ (10) Number of shortened iterations that trigger the watchdog. (see IPOPT documentation)
  | Watchdog_trial_iter_max Int -- ^ (3) Maximum number of watchdog iterations. (see IPOPT documentation)
  | Wsmp_iterative String -- ^ (no) Switches to iterative solver in WSMP. (see IPOPT documentation)
