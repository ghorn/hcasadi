{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.Bindings.NLP ( NLPSolverRaw
                           , c_deleteSolver
                           , c_createIpoptSolver
                           , c_solve
                           ) where

import Foreign.Ptr ( FunPtr, Ptr )

import Casadi.Bindings.SXM

data NLPSolverRaw

-- foreign imports
foreign import ccall unsafe "nlp.hpp &deleteSolver" c_deleteSolver :: FunPtr (Ptr NLPSolverRaw -> IO ())
foreign import ccall unsafe "nlp.hpp createIpoptSolver" c_createIpoptSolver
  :: Ptr SXMRaw -> Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr NLPSolverRaw)
foreign import ccall unsafe "nlp.hpp solve" c_solve :: Ptr NLPSolverRaw -> IO ()
