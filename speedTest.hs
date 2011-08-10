-- speedTest.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Time.Clock
import Hom
import Ddp
import Numeric.AD

-- an arbitrary timestep
cpDt :: Floating a => a
cpDt = 0.025


-- the dimensions
nx, nu :: Int
nx = 4
nu = 1


-- the differential equation
cpDxdt :: Floating a => State a -> Action a -> State a
cpDxdt state action = state'
  where
    [_, x', theta, theta'] = state
    [u] = action
    
    -- constants
    g = 9.8;
    len = 2.2
    mc = 2;
    mp = 1;

    x'' = 1/(mc+mp*sin(theta)*sin(theta))*(u+mp*sin(theta)*(len*theta'*theta'+g*cos(theta)))
    theta'' = 1/(len*(mc+mp*sin(theta)*sin(theta)))*(-u*cos(theta) - mp*len*theta'*theta'*cos(theta)*sin(theta) - (mc+mp)*g*sin(theta));

    state' = [x', x'', theta', theta'']


-- discrete differential equation using rk4 integration
dode :: Floating a => State a -> Action a -> State a
dode x u = rk4Step cpDxdt x u cpDt


-- the cost function
cpCost :: Floating a => State a -> Action a -> a
cpCost state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u + cos(x*x'*theta*theta'*u)
  where
    [x, x', theta, theta'] = state
    [u] = action


-- an arbitrary quadratic value function
cpQuad :: Num a => Quad a
cpQuad = Quad vxx vx v0 x0
  where
    vxx = [[1,2,3,4],
           [2,5,6,7],
           [3,6,8,9],
           [4,7,9,10]]
    vx = [10,20,30,40]
    v0 = 42
    x0 = [11,22]
 

-- function to benchmark
runComputation :: [Double] -> IO ()
runComputation xu = do
  let x = take nx xu
      u = drop nx xu
      
      liftQuad :: (Mode s, Num a) => Quad a -> Quad (s a)
      liftQuad (Quad vxx vx v0 x0) = Quad vxx' vx' v0' x0'
        where
          vxx' = map (map lift) vxx
          vx'  = map lift vx
          v0'  = lift v0
          x0'  = map lift x0
  print $ unprobe $ q0 cpCost dode (map lift x) (map lift u) (liftQuad cpQuad)
  print $ qx cpCost dode x u cpQuad
  print $ qu cpCost dode x u cpQuad
  print $ qxu cpCost dode x u cpQuad
  print $ qxx cpCost dode x u cpQuad
  print $ quu cpCost dode x u cpQuad
  putStrLn ""


-- time "runComputation"
timeOneRun :: [Double] -> IO (Double)
timeOneRun xu = do
  time0 <- getCurrentTime
  runComputation xu
  time1 <- getCurrentTime
  return (realToFrac (diffUTCTime time1 time0))


main :: IO ()
main = do
  let n = 100 :: Int -- number of times to call benchmarking function
      
      -- a bunch of arbitrary inputs
      xuInputs :: [[Double]]
      xuInputs = map (\k -> map fromIntegral [k..k+nx+nu-1]) [1..n::Int]

  times <- mapM timeOneRun xuInputs
  
  putStrLn $ "times: " ++ (show times)
  putStrLn $ "mean time: " ++ (show ((sum times)/(fromIntegral (length times))))
