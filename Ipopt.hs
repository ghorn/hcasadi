-- Ipopt.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Ipopt
       (
         ipoptSolverCreate
       , ipoptSolve
       ) where

import Casadi.SX
import Casadi.SXMatrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal(newArray, mallocArray, peekArray)
import Control.Exception(mask_)

-- the SXFunction data type
data IpoptSolverRaw = IpoptSolverRaw
newtype IpoptSolver = IpoptSolver (ForeignPtr IpoptSolverRaw)

-- foreign imports
foreign import ccall unsafe "ipoptSolverCreate" c_ipoptSolverCreate :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr IpoptSolverRaw)
foreign import ccall unsafe "&ipoptSolverDelete" c_ipoptSolverDelete :: FunPtr (Ptr IpoptSolverRaw -> IO ())
foreign import ccall unsafe "ipoptSolverSolve" c_ipoptSolverSolve :: Ptr IpoptSolverRaw -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble

------------------ creation -------------------------
ipoptSolverCreate :: SXMatrix -> SX -> [SX] -> IO IpoptSolver
ipoptSolverCreate (SXMatrix inputs) (SX objFun) constraintsList = mask_ $ do
  let (SXMatrix constraints) = sxMatrixFromList constraintsList
  ss <- withForeignPtrs3 c_ipoptSolverCreate inputs objFun constraints >>= newForeignPtr c_ipoptSolverDelete
  return $ IpoptSolver ss

ipoptSolve :: IpoptSolver -> [Double] -> ([Double],[Double]) -> ([Double],[Double]) -> IO ([Double], Double)
ipoptSolve (IpoptSolver solver) x0' (xLb', xUb') (gLb',gUb') = do
  x0  <- newArray $ map realToFrac x0'
  xLb <- newArray $ map realToFrac xLb'
  xUb <- newArray $ map realToFrac xUb'
  gLb <- newArray $ map realToFrac gLb'
  gUb <- newArray $ map realToFrac gUb'
  sol <- mallocArray (length x0')
  optVal <- withForeignPtr solver (\s -> c_ipoptSolverSolve s x0 xLb xUb gLb gUb sol)
  sol' <- peekArray (length x0') sol
  return $ (map realToFrac sol', realToFrac optVal)
