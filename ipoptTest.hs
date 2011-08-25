-- ipoptTest.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Ipopt
import Casadi

rosenbrock :: Floating a => [a] -> a
rosenbrock [x0,x1] = (1-x0)*(1-x0) + 100*(x1-x0*x0)*(x1-x0*x0)
rosenbrock _ = error "wrong number of inputs to rosenbrock"

-- box constraints only rosenbrock
solveRosenbrock :: IO ([Double], Double)
solveRosenbrock = do
  let x = sxMatrixSymbolic "x" (2,1)
      [x0,x1] = toList x
      constraints = []
      objFun = rosenbrock $ [x0,x1]
  
      xGuess = [-0.9, 1.0]
      xLb = [-10,-10]
      xUb = [10,10]
      
      gLb = []
      gUb = []

  solver <- ipoptSolverCreate x objFun constraints
  sol <- ipoptSolve solver xGuess (xLb, xUb) (gLb, gUb)
  return sol
  
  
-- quadratic with y >= 1 - x
solveQuadratic :: IO ([Double], Double)
solveQuadratic = do
  let x = sxMatrixSymbolic "x" (2,1)
      [x0,x1] = toList x
      constraints = [1 - x0 - x1]
      objFun = x0*x0 + x1*x1
  
      xGuess = [0.0, 0.0]
      xLb = [-10,-10]
      xUb = [10,10]
      
      gLb = [-1e12]
      gUb = [0]

  solver <- ipoptSolverCreate x objFun constraints
  sol <- ipoptSolve solver xGuess (xLb, xUb) (gLb, gUb)
  return sol


main :: IO ()
main = do
  (rSol,rVal) <- solveRosenbrock
  (qSol,qVal) <- solveQuadratic
  
  putStrLn "\n"
  putStrLn $ "rosenbrock sol: " ++ (show rSol) ++ ", optimal value: " ++ (show rVal)
  putStrLn $ "quadratic  sol: " ++ (show qSol) ++ ", optimal value: " ++ (show qVal)
