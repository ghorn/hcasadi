-- NLP.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}

module NLP.NLP
       (
         NLPSolver(..)
       , NLPRaw(..)
       , createSolver
       , solveNlp
       ) where

import Casadi
import Casadi.SX
import Casadi.SXMatrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal(newArray, mallocArray, peekArray)
import Control.Exception(mask_)
import Text.Printf


data NLPSolver a = NLPSolver { nlpPtr :: (ForeignPtr a)
                             , nInputs :: Int
                             , nConstraints :: Int
                             }

class NLPRaw a where
  c_createSolver :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr a)
  c_deleteSolver :: FunPtr (Ptr a -> IO ())
  c_solve :: Ptr a -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble

createSolver :: NLPRaw a => a -> SXMatrix -> SX -> SXMatrix -> IO (NLPSolver a)
createSolver _ (SXMatrix inputs) (SX objFun) (SXMatrix constraints) = mask_ $ do
  ss <- withForeignPtrs3 c_createSolver inputs objFun constraints >>= newForeignPtr c_deleteSolver
  return $ NLPSolver { nlpPtr = ss,  nInputs = rows (SXMatrix inputs), nConstraints = rows (SXMatrix constraints) }


solveNlp :: NLPRaw a => NLPSolver a -> [Double] -> ([Double],[Double]) -> ([Double],[Double]) -> IO ([Double], Double)
solveNlp nlp x0' (xLb', xUb') (gLb',gUb') =
  if any (\x -> (nInputs nlp) /= length x) [xLb',xUb'] || any (\x -> (nConstraints nlp) /= length x) [gLb',gUb']
    then error $ printf ("\nError - Bad dimensions in ipoptSolve\n" ++
                         "        Solve call saw nxLb = %d, nxUb = %d, ngLb = %d, ngUb = %d\n" ++
                         "        Solver has %d inputs and %d nonlcons")
         (length xLb') (length xUb') (length gLb') (length gUb') (nInputs nlp) (nConstraints nlp)
  else do
    x0  <- newArray $ map realToFrac x0'
    xLb <- newArray $ map realToFrac xLb'
    xUb <- newArray $ map realToFrac xUb'
    gLb <- newArray $ map realToFrac gLb'
    gUb <- newArray $ map realToFrac gUb'
    sol <- mallocArray (length x0')
    optVal <- withForeignPtr (nlpPtr nlp) (\s -> c_solve s x0 xLb xUb gLb gUb sol)
    sol' <- peekArray (length x0') sol
  
    return (map realToFrac sol', realToFrac optVal)
