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
import Casadi.DMatrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception(mask_)
import Text.Printf


data NLPSolver a = NLPSolver { nlpPtr :: (ForeignPtr a)
                             , nInputs :: Int
                             , nConstraints :: Int
                             }

class NLPRaw a where
  c_createSolver :: Ptr SXMatrixRaw -> Ptr SXRaw -> Ptr SXMatrixRaw -> IO (Ptr a)
  c_deleteSolver :: FunPtr (Ptr a -> IO ())
  c_solve :: Ptr a -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> Ptr DMatrixRaw -> IO CDouble

createSolver :: NLPRaw a => a -> SXMatrix -> SX -> SXMatrix -> IO (NLPSolver a)
createSolver _ (SXMatrix inputs) (SX objFun) (SXMatrix constraints) = mask_ $ do
  ss <- withForeignPtrs3 c_createSolver inputs objFun constraints >>= newForeignPtr c_deleteSolver
  return $ NLPSolver { nlpPtr = ss,  nInputs = rows (SXMatrix inputs), nConstraints = rows (SXMatrix constraints) }


solveNlp :: NLPRaw a => NLPSolver a -> DMatrix -> (DMatrix,DMatrix) -> (DMatrix,DMatrix) -> IO (DMatrix, Double)
solveNlp nlp
  x0'@(DMatrix x0Raw)
  (xLb'@(DMatrix xLbRaw), xUb'@(DMatrix xUbRaw))
  (gLb'@(DMatrix gLbRaw), gUb'@(DMatrix gUbRaw)) =
    if any (\x -> (nInputs nlp) /= rows x) [x0',xLb',xUb'] || any (\x -> (nConstraints nlp) /= rows x) [gLb',gUb']
      then error $ printf ("\nError - Bad dimensions in ipoptSolve\n" ++
                           "        Solve call saw nx = %d, nxLb = %d, nxUb = %d, ngLb = %d, ngUb = %d\n" ++
                           "        Solver has %d inputs and %d nonlcons")
           (rows x0') (rows xLb') (rows xUb') (rows gLb') (rows gUb') (nInputs nlp) (nConstraints nlp)
    else do
      DMatrix solRaw <- dMatrixNewZeros $ size (DMatrix x0Raw)

      let x0  = unsafeForeignPtrToPtr x0Raw
          xLb = unsafeForeignPtrToPtr xLbRaw
          xUb = unsafeForeignPtrToPtr xUbRaw
          gLb = unsafeForeignPtrToPtr gLbRaw
          gUb = unsafeForeignPtrToPtr gUbRaw
          sol = unsafeForeignPtrToPtr solRaw
      
      optVal <- withForeignPtr (nlpPtr nlp) (\s -> c_solve s x0 xLb xUb gLb gUb sol)
      
      touchForeignPtr x0Raw
      touchForeignPtr xLbRaw
      touchForeignPtr xUbRaw
      touchForeignPtr gLbRaw
      touchForeignPtr gUbRaw
      touchForeignPtr solRaw
      
      return (DMatrix solRaw, realToFrac optVal)
