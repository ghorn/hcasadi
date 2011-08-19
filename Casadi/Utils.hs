-- Utils.hs

{-# OPTIONS_GHC -Wall #-}

module Casadi.Utils
       (
         getDerivs
       ) where

import Casadi.SX
import Casadi.SXMatrix
import Casadi.SXFunction
import Numeric.LinearAlgebra
import System.IO.Unsafe(unsafePerformIO)

getDerivs :: ([SX] -> SX) -> Integer
             -> IO (Vector Double -> Double, Vector Double -> Vector Double, Vector Double -> Matrix Double)
getDerivs f n = do
  
  x <- sxMatrixCreateSymbolic "x" (n,1)
      
  let xSX   = sxMatrixToList x
      sxFun = sxFunctionCreate [x] [sxMatrixFromList [f xSX]]
      
      gradSX  = sxFunctionGradientAt sxFun 0
      hessSX  = sxFunctionHessianAt sxFun (0,0)

      sxGrad = sxFunctionCreate [x] [gradSX]
      sxHess = sxFunctionCreate [x] [hessSX]
      
  return (evalF sxFun, evalG sxGrad, evalH sxHess)

evalF :: SXFunction -> Vector Double -> Double
evalF sxFun xVec = unsafePerformIO $ do
  [[[output]]] <- sxFunctionEvaluate sxFun [[toList xVec]]
  return $ output

evalG :: SXFunction -> Vector Double -> Vector Double
evalG sxGrad xVec = unsafePerformIO $ do
  [output] <- sxFunctionEvaluate sxGrad [[toList xVec]]
  return $ fromList (concat output)

evalH :: SXFunction -> Vector Double -> Matrix Double
evalH sxHess xVec = unsafePerformIO $ do
  [output] <- sxFunctionEvaluate sxHess [[toList xVec]]
  return $ fromLists output
