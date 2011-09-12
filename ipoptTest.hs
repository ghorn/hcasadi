-- ipoptTest.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import NLP.NLP
--import NLP.Ipopt
import NLP.Snopt
import Casadi

rosenbrock :: Matrix a b c => a -> b
rosenbrock inputs = (1-x0)*(1-x0) + 100*(x1-x0*x0)*(x1-x0*x0)
  where
    [x0,x1] = toList inputs

-- box constraints only rosenbrock
solveRosenbrock :: IO (DMatrix, Double)
solveRosenbrock = do
  let x = sxMatrixSymbolic "x" (2,1)
      objFun = rosenbrock x
      constraints = fromList []
  
      xGuess = fromList [-0.9, 1.0]
      xLb = fromList [-10,-10]
      xUb = fromList [10,10]

      gLb = fromList []
      gUb = fromList []

--  solver <- createSolver IpoptExactHessian x objFun constraints
  solver <- createSolver Snopt x objFun constraints
  sol <- solveNlp solver xGuess (xLb, xUb) (gLb, gUb)
  return sol
  
  
-- quadratic with y >= 1 - x
solveQuadratic :: IO (DMatrix, Double)
solveQuadratic = do
  let x = sxMatrixSymbolic "x" (2,1)
      [x0,x1] = toList x
      objFun = x0*x0 + x1*x1
      constraints = fromList [1 - x0 - x1]
  
      xGuess = fromList [0.0, 0.0]
      xLb = fromList [-10,-10]
      xUb = fromList [10,10]

      gLb = fromList [-1e25]
      gUb = fromList [0]

--  solver <- createSolver IpoptExactHessian x objFun constraints
  solver <- createSolver Snopt x objFun constraints
  sol <- solveNlp solver xGuess (xLb, xUb) (gLb, gUb)
  return sol


main :: IO ()
main = do
  (rSol,rVal) <- solveRosenbrock
  (qSol,qVal) <- solveQuadratic
  
  putStrLn "\n"
  putStrLn $ "rosenbrock sol: " ++ (show rSol) ++ ", optimal value: " ++ (show rVal)
  putStrLn $ "quadratic sol: " ++ (show qSol) ++ ", optimal value: " ++ (show qVal)
