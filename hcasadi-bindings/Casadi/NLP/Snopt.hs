-- Snopt.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.NLP.Snopt
       (
         Snopt(..)
       ) where

import Casadi.SX
import Casadi.SXMatrix
import Casadi.DMatrix
import Casadi.NLP

import Foreign.C
import Foreign.Ptr

data Snopt = Snopt

instance NLPRaw Snopt where
  c_createSolver = c_snoptSolverCreate
  c_deleteSolver = c_snoptSolverDelete
  c_solve = c_snoptSolverSolve

-- foreign imports
foreign import ccall unsafe "snoptSolverCreate" c_snoptSolverCreate :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr Snopt)
foreign import ccall unsafe "&snoptSolverDelete" c_snoptSolverDelete :: FunPtr (Ptr Snopt -> IO ())
foreign import ccall unsafe "snoptSolverSolve" c_snoptSolverSolve :: Ptr Snopt -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> IO CDouble
