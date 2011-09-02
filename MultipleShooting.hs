-- MultipleShooting.hs

{-# OPTIONS_GHC -Wall #-}

module MultipleShooting( Ode(..)
                       , Cost(..)
                       , simpleSystem
                       , multipleShooting
                       , MultipleShooting(..)
                       , boundEq
                       , boundEqs
                       , boundInterval
                       , boundIntervals
                       , multipleShootingSolver
                       , eqZero
                       , ltZero
                       , devectorize
                       ) where

import Casadi
--import NLP.Ipopt
import NLP.Snopt
import NLP.NLP
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

devectorize :: [Double] -> MultipleShooting -> ([[Double]], [[Double]], [Double])
devectorize sol ms = (xTraj, uTraj, params')
  where
    (xDims, uDims) = unzip $ map (\(Cost _ d) -> d) (costs (system ms))
    (sol', xTraj) = mapAccumL f sol xDims
    (params', uTraj) = mapAccumL f sol' uDims

    f acc n = (xs, x)
      where
        (x,xs) = splitAt n acc

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

boundInterval :: MultipleShooting -> SX -> (Double, Double) -> Bound
boundInterval ms x (lb, ub) = Bound { dvIdx = idxOfMat x (designVars ms)
                                    , lbound = lb
                                    , ubound = ub
                                    , var = x
                                    }

boundIntervals :: MultipleShooting -> SXMatrix -> [(Double,Double)] -> [Bound]
boundIntervals ms xs bnds = zipWith (boundInterval ms) (toList xs) bnds


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

--  solver <- createSolver IpoptExactHessian (designVars ms) (objFun ms) (concatMat $ map expression allConstraints)
  solver <- createSolver Snopt (designVars ms) (objFun ms) (concatMat $ map expression allConstraints)
  
  let solve :: [Bound] -> [Double] -> IO ([Double], Double)
      solve bounds xGuess = solveNlp solver xGuess (boxLbs, boxUbs) (nlLbs, nlUbs)
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
