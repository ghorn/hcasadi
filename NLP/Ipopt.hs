-- Ipopt.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module NLP.Ipopt
       (
         Ipopt(..)
       , IpoptExactHessian(..)
       ) where

import Casadi.SX
import Casadi.SXMatrix
import Casadi.DMatrix
import NLP.NLP

import Foreign.C
import Foreign.Ptr

data Ipopt = Ipopt
data IpoptExactHessian = IpoptExactHessian

instance NLPRaw Ipopt where
  c_createSolver = c_ipoptSolverCreate
  c_deleteSolver = c_ipoptSolverDelete
  c_solve = c_ipoptSolverSolve
instance NLPRaw IpoptExactHessian where
  c_createSolver = c_ipoptSolverCreateExactHessian
  c_deleteSolver = c_ipoptSolverDeleteExactHessian
  c_solve = c_ipoptSolverSolveExactHessian


-- foreign imports
foreign import ccall unsafe "ipoptSolverCreate" c_ipoptSolverCreate :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr Ipopt)
foreign import ccall unsafe "ipoptSolverCreateExactHessian" c_ipoptSolverCreateExactHessian :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr IpoptExactHessian)
foreign import ccall unsafe "&ipoptSolverDelete" c_ipoptSolverDelete :: FunPtr (Ptr Ipopt -> IO ())
foreign import ccall unsafe "&ipoptSolverDelete" c_ipoptSolverDeleteExactHessian :: FunPtr (Ptr IpoptExactHessian -> IO ())
foreign import ccall unsafe "ipoptSolverSolve" c_ipoptSolverSolve :: Ptr Ipopt -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> IO CDouble
foreign import ccall unsafe "ipoptSolverSolve" c_ipoptSolverSolveExactHessian :: Ptr IpoptExactHessian -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> IO CDouble
