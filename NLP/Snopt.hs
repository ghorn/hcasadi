-- Snopt.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module NLP.Snopt
       (
         Snopt(..)
       ) where

import Casadi.SX
import Casadi.SXMatrix
import NLP.NLP

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
foreign import ccall unsafe "snoptSolverSolve" c_snoptSolverSolve :: Ptr Snopt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble
