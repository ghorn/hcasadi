-- optTest.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Casadi.Utils
import Optimization.QuasiNewton
import Numeric.LinearAlgebra

import Graphics.Gnuplot.Simple

rosenbrock :: Floating a => [a] -> a
rosenbrock [x0,x1] = (1-x0)*(1-x0) + 100*(x1-x0*x0)*(x1-x0*x0)
rosenbrock _ = error "wrong number of inputs to rosenbrock"

main :: IO ()
main = do
  (rosenbrockF, rosenbrockG, _) <- getDerivs rosenbrock 2
  let x0 = fromList [-0.9,1] :: Vector Double
--      results = (take 20 $ dfp x0 rosenbrockF rosenbrockG)
      results = (take 20 $ bfgs x0 rosenbrockF rosenbrockG)
      path = map f results
        where
          f (vec,_) = (x,y)
            where
              [x,y] = toList vec
  mapM_ (\(_,v) -> print $ inv v) results
  mapM_ (\(x,_) -> print x) results

  plotList [] path
