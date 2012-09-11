{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Vector.Storable as V

import Dvda
import Casadi.Dvda
import Casadi.SXM ( SXM, sxmToList )
import Casadi.NLPSolver
import Casadi.NLPSolverOptions

model :: IO ([SXM], [SXM])
model = toCasadi [x,y,z] (sqr (x+0.3) + sqr (x+0.6) + sqr (x+0.9))
  where
    x = sym "x"
    y = sym "y"
    z = sym "z"
    sqr w = w*w

main :: IO ()
main = do
  ([dvs], [objFun]) <- model
  [x,y,z] <- sxmToList dvs

  let constraints = x
  --  constraints <- sxmVecCat []

  putStrLn $ "dvs: " ++ show dvs
  putStrLn $ "objFun: " ++ show objFun
  putStrLn $ "[x,y,z]: " ++ show [x,y,z]

  solver <- createIpoptSolver dvs objFun constraints [Monitor ["eval_f"]]

  let lbs = V.fromList [-2,-2,-2]
      ubs = V.fromList [2,2,2]
      gmin = V.fromList [-200]
      gmax = V.fromList [200]
--      gmin = V.fromList []
--      gmax = V.fromList []
      guess = V.fromList [0.5,0.5,0.5]
  sol <- solveNlp solver (lbs,ubs) (gmin, gmax) guess

  putStrLn $ "solution:\n" ++ show sol
  putStrLn "done"
