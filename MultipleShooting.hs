-- MultipleShooting.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Casadi
import Ipopt
import Text.Printf
import Data.List
import Data.Maybe

data Ode = Ode (SXMatrix -> SXMatrix -> SXMatrix) (Int,Int)
data Cost = Cost (SXMatrix -> SXMatrix -> SX) (Int,Int)

data Constraint = Constraint { expression :: SXMatrix
                             , lcon :: [Double]
                             , ucon :: [Double]
                             } deriving Show

data Bound = Bound { dvIdx :: Int
                   , lbound :: Double
                   , ubound :: Double
                   , var :: SX
                   } deriving Show

data System = System { odes :: [Ode]
                     , costs :: [Cost]
                     , dts :: [SX]
                     }

data MultipleShooting = MultipleShooting { system :: System
                                         , states :: [SXMatrix]
                                         , actions :: [SXMatrix]
                                         , params :: SXMatrix
                                         , dodeConstraints :: [Constraint]
                                         , objFun :: SX
                                         , designVars :: SXMatrix
                                         }

idxOfMat :: SX -> SXMatrix -> Int
idxOfMat val mat
  | isJust idx = fromJust idx
  | otherwise  = error $ "Error - idxOfMat could not find \"" ++ (show val) ++ "\" in " ++ (show mat)
  where
    idx = elemIndex val (toList mat)


boundEq :: MultipleShooting -> SX -> Double -> Bound
boundEq ms x val = Bound { dvIdx = idxOfMat x (designVars ms)
                         , lbound = val
                         , ubound = val
                         , var = x
                         }


boundEqs :: MultipleShooting -> SXMatrix -> [Double] -> [Bound]
boundEqs ms xs vals = zipWith (boundEq ms) (toList xs) vals


multipleShooting :: System -> SXMatrix -> MultipleShooting
multipleShooting sys params'
  | dimensionsMatch = MultipleShooting { system = sys
                                       , states = states'
                                       , actions = actions'
                                       , params = params'
                                       , dodeConstraints = dcs
                                       , objFun = objFun'
                                       , designVars = dvs
                                       }
  | otherwise = error $ printf "Error in multipleShooting: lengths of odes (%d), costs (%d), dts (%d) are not consistent" nOdes nCosts nDts
  where
    dimensionsMatch = (nOdes == nDts) && (nCosts == nOdes + 1) && (and $ zipWith (==) odeDims costDims)

    odeDims = map (\(Ode _ d) -> d) (odes sys)
    costDims = map (\(Cost _ d) -> d) (costs sys)

    nOdes = length (odes sys)
    nCosts = length (costs sys)
    nDts = length (dts sys)

    states'  = zipWith (\(nx,_) k -> sxMatrixSymbolic ("x"++(show k)) (nx, 1)) costDims [0..nCosts-1]
    actions' = zipWith (\(_,nu) k -> sxMatrixSymbolic ("u"++(show k)) (nu, 1)) costDims [0..nCosts-1]

    dcs = map eqZero $ zipWith6 (simpsonsRuleError) (init states') (init actions') (tail states') (tail actions') (odes sys) (dts sys)

    objFun' = sum $ zipWith3 (\(Cost cost _) x u -> cost x u) (costs sys) states' actions'

    dvs = concatMat [concatMat states', concatMat actions', params']

multipleShootingSolver :: MultipleShooting -> [Constraint] -> IO ([Bound] -> [Double] -> IO ([Double], Double))
multipleShootingSolver ms moreConstraints = do
  let allConstraints = concat [dodeConstraints ms, moreConstraints]

  solver <- ipoptSolverCreate (designVars ms) (objFun ms) (concatMat $ map expression allConstraints)
  
  let solve :: [Bound] -> [Double] -> IO ([Double], Double)
      solve bounds xGuess = ipoptSolve solver xGuess (boxLbs, boxUbs) (nlLbs, nlUbs)
        where
          nlLbs = concat $ map lcon allConstraints
          nlUbs = concat $ map ucon allConstraints
          (boxLbs, boxUbs) = unzip $ map f [0..rows (designVars ms)-1]
          f idx
            | isNothing bnd = (-1e50, 1e50)
            | otherwise     = (lbound (fromJust bnd), ubound (fromJust bnd))
              where
                bnd = find (\x -> idx == dvIdx x) bounds

  return solve

simpleSystem :: Ode -> Cost -> SX -> Int -> System
simpleSystem ode cost dt n = System { odes = replicate (n-1) ode
                                    , costs = replicate n cost
                                    , dts = replicate (n-1) dt
                                    }

simpsonsRuleError :: SXMatrix -> SXMatrix -> SXMatrix -> SXMatrix -> Ode -> SX -> SXMatrix
simpsonsRuleError xk uk xkp1 ukp1 (Ode ode _) dt = xkp1 - xk - ((dt/6.0) <> (f0 + fourFm + f1))
  where
    f0 = ode xk uk
    f1 = ode xkp1 ukp1

    um = 0.5*(uk + ukp1)
    xm = xm' - xm''
      where
        xm' = 0.5*(xk + xkp1)
        xm'' = (0.125*dt) <> (f1 - f0)

    fm = ode xm um
    fourFm = 4*fm

eqZero :: SXMatrix -> Constraint
eqZero g = Constraint {expression = g, lcon = zeros, ucon = zeros}
  where
    zeros = replicate (rows g) 0


ltZero :: SXMatrix -> Constraint
ltZero g = Constraint {expression = g, lcon = veryNegative, ucon = zeros}
  where
    veryNegative = replicate (rows g) (-1e30)
    zeros = replicate (rows g) 0


-- ode
cartpoleOde :: Ode
cartpoleOde = Ode cartpoleDxdt (4,1)
    
cartpoleDxdt :: SXMatrix -> SXMatrix -> SXMatrix
cartpoleDxdt state action = state'
  where
    [_, x', theta, theta'] = toList state
    [u] = toList action
    
    -- constants
    g = 9.8;
    len = 2.2
    mc = 2;
    mp = 1;

    x'' = 1/(mc+mp*sin(theta)*sin(theta))*(u+mp*sin(theta)*(len*theta'*theta'+g*cos(theta)))
    theta'' = 1/(len*(mc+mp*sin(theta)*sin(theta)))*(-u*cos(theta) - mp*len*theta'*theta'*cos(theta)*sin(theta) - (mc+mp)*g*sin(theta));

    state' = fromList [x', x'', theta', theta'']


-- cost fcn
cpCost :: Cost
cpCost = Cost cpCost' (4,1)

cpCost' :: SXMatrix -> SXMatrix -> SX
cpCost' state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u -- + barrier
  where
    [x, x', theta, theta'] = toList state
    [u] = toList action

--    -- barrier
--    uUb =  10.1
--    uLb = -10.1
--    mu = 1.0
--    uBarrierUb = -mu*log(  uUb - u )
--    uBarrierLb = -mu*log( -uLb + u )
--    barrier = uBarrierUb + uBarrierLb


main :: IO ()
main = do
  let n = 60
--      np = 1

      tEnd =  sxSymbolic "tEnd"
      dt = tEnd/(sxInt (n-1))

      sys = simpleSystem cartpoleOde cpCost dt n
      ms = multipleShooting sys (fromList [tEnd])

--      x0 = replicate n [-10,0,0.01,0::Double]
--      u0 = replicate n [0::Double]
      xGuess = (replicate ((rows $ designVars ms) - 1) (1.0::Double))++[10::Double]
  
      x0Sx = head $ states ms
      xfSx = last $ states ms
      
      bounds = concat [ boundEqs ms x0Sx [-10, 0, 0.01, 0]
                      , boundEqs ms xfSx [0,0,pi,0]
                      , [boundEq ms tEnd 10]
                      ]

  msSolve <- multipleShootingSolver ms []
  (sol0,_) <- msSolve bounds xGuess
  (sol,_) <- msSolve bounds sol0
  
  print $ length sol

