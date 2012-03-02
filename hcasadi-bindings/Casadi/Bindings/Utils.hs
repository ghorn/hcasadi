-- Utils.hs

{-# OPTIONS_GHC -Wall #-}

module Casadi.Utils
       (
         getDerivs
       , timeComputation
       ) where

import Casadi.SX
import Casadi.SXMatrix
import Casadi.Matrix
import Casadi.SXFunction
import qualified Numeric.LinearAlgebra as LA
import Data.Time.Clock

getDerivs :: ([SX] -> SX) -> Int
             -> IO (LA.Vector Double -> Double, LA.Vector Double -> LA.Vector Double, LA.Vector Double -> LA.Matrix Double)
getDerivs f n = do
  
  x <- sxMatrixCreateSymbolic "x" (n,1)
      
  let xSX   = toList x
      sxFun = sxFunctionCreate [x] [fromList [f xSX]]
      
      gradSX  = sxFunctionGradientAt sxFun 0
      hessSX  = sxFunctionHessianAt sxFun (0,0)

      sxGrad = sxFunctionCreate [x] [gradSX]
      sxHess = sxFunctionCreate [x] [hessSX]
      
  return (evalF sxFun, evalG sxGrad, evalH sxHess)

evalF :: SXFunction -> LA.Vector Double -> Double
evalF sxFun xVec = output
  where
    [[[output]]] = sxFunctionEvaluateLists sxFun [[LA.toList xVec]]

evalG :: SXFunction -> LA.Vector Double -> LA.Vector Double
evalG sxGrad xVec = LA.fromList (concat output)
  where
    [output] = sxFunctionEvaluateLists sxGrad [[LA.toList xVec]]

evalH :: SXFunction -> LA.Vector Double -> LA.Matrix Double
evalH sxHess xVec = LA.fromLists output
  where
    [output] = sxFunctionEvaluateLists sxHess [[LA.toList xVec]]


timeComputation :: String -> IO t -> IO t
timeComputation msg a = do
    start <- getCurrentTime
    v <- a
    end   <- getCurrentTime
    let diffTime = (realToFrac $ diffUTCTime end start)::Double
    putStrLn $ msg ++ show diffTime ++ " seconds"
    return v
