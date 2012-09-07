{-# OPTIONS_GHC -Wall #-}

module Casadi.SXFunctionOptionsInternal ( sxFunctionUnsafeSetOption
                                        ) where

import Casadi.Types ( SXFunction )
import Casadi.Options ( Option(..) )
import Casadi.SXFunctionOptions ( SXFunctionOption(..) )

unsafeSetOption' :: Option a => SXFunction -> String -> a -> IO ()
unsafeSetOption' = unsafeSetOption

sxFunctionUnsafeSetOption :: SXFunction -> SXFunctionOption -> IO ()
sxFunctionUnsafeSetOption fun (Ad_mode x) = unsafeSetOption' fun "ad_mode" x
sxFunctionUnsafeSetOption fun (Inplace x) = unsafeSetOption' fun "inplace" x
sxFunctionUnsafeSetOption fun (Jac_for_sens x) = unsafeSetOption' fun "jac_for_sens" x
sxFunctionUnsafeSetOption fun (Just_in_time x) = unsafeSetOption' fun "just_in_time" x
sxFunctionUnsafeSetOption fun (Live_variables x) = unsafeSetOption' fun "live_variables" x
sxFunctionUnsafeSetOption fun (Monitor x) = unsafeSetOption' fun "monitor" x
sxFunctionUnsafeSetOption fun (Name x) = unsafeSetOption' fun "name" x
sxFunctionUnsafeSetOption fun (Number_of_adj_dir x) = unsafeSetOption' fun "number_of_adj_dir" x
sxFunctionUnsafeSetOption fun (Number_of_fwd_dir x) = unsafeSetOption' fun "number_of_fwd_dir" x
sxFunctionUnsafeSetOption fun (Numeric_hessian x) = unsafeSetOption' fun "numeric_hessian" x
sxFunctionUnsafeSetOption fun (Numeric_jacobian x) = unsafeSetOption' fun "numeric_jacobian" x
sxFunctionUnsafeSetOption fun (Sparse x) = unsafeSetOption' fun "sparse" x
sxFunctionUnsafeSetOption fun (Store_jacobians x) = unsafeSetOption' fun "store_jacobians" x
sxFunctionUnsafeSetOption fun (Topological_sorting x) = unsafeSetOption' fun "topological_sorting" x
sxFunctionUnsafeSetOption fun (Verbose x) = unsafeSetOption' fun "verbose" x
