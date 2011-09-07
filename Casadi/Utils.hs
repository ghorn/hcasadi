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

getDerivs :: ([SX] -> SX) -> Int
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
evalF sxFun xVec = output
  where
    [[[output]]] = sxFunctionEvaluateLists sxFun [[toList xVec]]

evalG :: SXFunction -> Vector Double -> Vector Double
evalG sxGrad xVec = fromList (concat output)
  where
    [output] = sxFunctionEvaluateLists sxGrad [[toList xVec]]

evalH :: SXFunction -> Vector Double -> Matrix Double
evalH sxHess xVec = fromLists output
  where
    [output] = sxFunctionEvaluateLists sxHess [[toList xVec]]
