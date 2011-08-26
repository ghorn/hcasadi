-- Ipopt.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Ipopt
       (
         ipoptSolverCreate
       , ipoptSolverCreateExactHessian
       , ipoptSolve
       , IpoptSolverRaw(..)
       ) where

import Casadi
import Casadi.SX
import Casadi.SXMatrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal(newArray, mallocArray, peekArray)
import Control.Exception(mask_)
import Text.Printf

-- the SXFunction data type
data IpoptSolverRaw = IpoptSolverRaw
data IpoptSolver = IpoptSolver (ForeignPtr IpoptSolverRaw) Int Int

-- foreign imports
foreign import ccall unsafe "ipoptSolverCreate" c_ipoptSolverCreate :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr IpoptSolverRaw)
foreign import ccall unsafe "ipoptSolverCreateExactHessian" c_ipoptSolverCreateExactHessian :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr IpoptSolverRaw)
foreign import ccall unsafe "&ipoptSolverDelete" c_ipoptSolverDelete :: FunPtr (Ptr IpoptSolverRaw -> IO ())
foreign import ccall unsafe "ipoptSolverSolve" c_ipoptSolverSolve :: Ptr IpoptSolverRaw -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble

------------------ creation -------------------------
ipoptSolverCreate :: SXMatrix -> SX -> SXMatrix -> IO IpoptSolver
ipoptSolverCreate (SXMatrix inputs) (SX objFun) (SXMatrix constraints) = mask_ $ do
  ss <- withForeignPtrs3 c_ipoptSolverCreate inputs objFun constraints >>= newForeignPtr c_ipoptSolverDelete
  return $ IpoptSolver ss (rows (SXMatrix inputs)) (rows (SXMatrix constraints))

ipoptSolverCreateExactHessian :: SXMatrix -> SX -> SXMatrix -> IO IpoptSolver
ipoptSolverCreateExactHessian (SXMatrix inputs) (SX objFun) (SXMatrix constraints) = mask_ $ do
  ss <- withForeignPtrs3 c_ipoptSolverCreateExactHessian inputs objFun constraints >>= newForeignPtr c_ipoptSolverDelete
  return $ IpoptSolver ss (rows (SXMatrix inputs)) (rows (SXMatrix constraints))

ipoptSolve :: IpoptSolver -> [Double] -> ([Double],[Double]) -> ([Double],[Double]) -> IO ([Double], Double)
ipoptSolve (IpoptSolver solver nInputs nG) x0' (xLb', xUb') (gLb',gUb') =
  if any (\x -> nInputs /= length x) [xLb',xUb'] || any (\x -> nG /= length x) [gLb',gUb']
    then error $ printf ("\nError - Bad dimensions in ipoptSolve\n" ++
                         "        Solve call saw nxLb = %d, nxUb = %d, ngLb = %d, ngUb = %d\n" ++
                         "        Solver has %d inputs and %d nonlcons")
         (length xLb') (length xUb') (length gLb') (length gUb') nInputs nG
  else do
    x0  <- newArray $ map realToFrac x0'
    xLb <- newArray $ map realToFrac xLb'
    xUb <- newArray $ map realToFrac xUb'
    gLb <- newArray $ map realToFrac gLb'
    gUb <- newArray $ map realToFrac gUb'
    sol <- mallocArray (length x0')
    optVal <- withForeignPtr solver (\s -> c_ipoptSolverSolve s x0 xLb xUb gLb gUb sol)
    sol' <- peekArray (length x0') sol
  
    return (map realToFrac sol', realToFrac optVal)
