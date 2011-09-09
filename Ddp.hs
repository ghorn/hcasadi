-- Ddp.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Ddp
       (
         prepareQFunction,
         evalQFunction,
         prepareDdp,
         BacksweepOutput
       ) where

import Casadi
import Hom(Cost, Ode, Quad(..), evalQuad)
import Data.List(mapAccumL)

type BacksweepOutput = (Quad DMatrix, DMatrix, DMatrix)

-- prepare casadi SXFunction
prepareQFunction :: Int -> Int -> Cost SXMatrix SX -> Ode SXMatrix -> SXFunction
prepareQFunction nx nu costFunction dode = sxFunction
                                           [x,u,vxx,vx,v0,x0]
                                           [fromList [q], qx, qu, qxx, qxu, quu]
  where
    -- inputs
    x   = sxMatrixSymbolic "x"   (nx,1)
    u   = sxMatrixSymbolic "u"   (nu,1)
    vxx = sxMatrixSymbolic "vxx" (nx,nx)
    vx  = sxMatrixSymbolic "vx"  (nx,1)
    v0  = sxMatrixSymbolic "v0"  (1,1)
    x0  = sxMatrixSymbolic "x0"  (nx,1)

    -- reshape SXMatrices into lists
    quad = Quad vxx vx v0 x0

    -- q function and its quadratic expansion
    q = (costFunction x u) + (evalQuad quad (dode x u))
    qx = gradient q x
    qu = gradient q u
    qxx = hessian q x
    quu = hessian q u
    qxu = jacobian qx u

prepareDodeFunction :: Int -> Int -> Ode SXMatrix -> SXFunction
prepareDodeFunction nx nu dode = sxFunction [x,u] [xNext]
  where
    x   = sxMatrixSymbolic "x"   (nx,1)
    u   = sxMatrixSymbolic "u"   (nu,1)
    xNext = dode x u

-- evaluate
evalQFunction :: SXFunction -> (DMatrix, DMatrix, Quad DMatrix) -> (DMatrix, DMatrix, DMatrix, DMatrix, DMatrix, DMatrix)
evalQFunction qExpansion (x, u, Quad vxx vx v0 x0) = qOut
  where
    [q,qx,qu,qxx,qxu,quu] = sxFunctionEvaluate qExpansion [x, u, vxx, vx, v0, x0]
    qOut = ( q, qx, qu, qxx, qxu, quu)

----------------------- convenience function -----------------------
-- prepare ddp  
prepareDdp :: Cost SXMatrix SX -> Ode SXMatrix -> Int -> Int -> [(Double,Double)]
              -> ([DMatrix] -> [DMatrix] -> [([DMatrix], [DMatrix], [BacksweepOutput])])
prepareDdp cost dode nx nu uBounds = ddp qFun dode' uBounds
  where
    qFun = prepareQFunction nx nu cost dode
    dodeFun = prepareDodeFunction nx nu dode
    dode' x u = head $ sxFunctionEvaluate dodeFun [x,u]
  
-- iterate ddp'
ddp :: SXFunction -> Ode DMatrix -> [(Double,Double)] -> [DMatrix] -> [DMatrix] -> [([DMatrix], [DMatrix], [BacksweepOutput])]
ddp qFun dode uBounds xTraj0 uTraj0 = iterate f x0
  where
    f (xTraj, uTraj, _) = ddp' qFun dode uBounds xTraj uTraj
    x0 = ddp' qFun dode uBounds xTraj0 uTraj0


-- just one backsweep then forwardsweep
ddp' :: SXFunction -> Ode DMatrix -> [(Double,Double)] -> [DMatrix] -> [DMatrix] -> ([DMatrix], [DMatrix], [BacksweepOutput])
ddp' qFunction dode uBounds xTraj0 uTraj0 = (xTraj, uTraj, backsweepTrajectory)
  where
    backsweepTrajectory :: [BacksweepOutput]
    backsweepTrajectory = backSweep qFunction xTraj0 uTraj0

    forwardsweepTrajectory :: [(DMatrix, DMatrix)]
    forwardsweepTrajectory = forwardSweep dode (head xTraj0) backsweepTrajectory uBounds
    (xTraj, uTraj) = unzip forwardsweepTrajectory


-------------------------- forward sweep -----------------------------
forwardSweep :: Ode DMatrix -> DMatrix -> [BacksweepOutput] -> [(Double,Double)]
                -> [(DMatrix, DMatrix)]
forwardSweep dode x0 backsweepTrajectory uBounds = snd $ mapAccumL (fSweep uBounds dode) x0 backsweepTrajectory


fSweep :: [(Double,Double)] -> Ode DMatrix -> DMatrix -> BacksweepOutput -> (DMatrix, (DMatrix, DMatrix))
fSweep uBounds dode x (Quad _ _ _ x0, feedbackGain, uOpenLoop) = (xNext, (x, u))
  where
    u = mimoController uBounds x x0 feedbackGain uOpenLoop
    xNext = dode x u

mimoController :: [(Double,Double)] -> DMatrix -> DMatrix -> DMatrix -> DMatrix -> DMatrix
mimoController uBounds x x0 feedbackMatrix uOpenLoop = u
  where
    u' = uOpenLoop + feedbackMatrix*(x - x0)
    u = fromList $ zipWith bound uBounds (toList u')
      where
        bound (lb,ub) u''
          | u'' < lb  = lb
          | u'' > ub  = ub
          | otherwise = u''


-------------------------- backward sweep --------------------------
backSweep :: SXFunction -> [DMatrix] -> [DMatrix] -> [BacksweepOutput]
backSweep qFunction xTraj0 uTraj0 = foldr (\x acc -> backSweep' qFunction x acc) [] (zip xTraj0 uTraj0)

backSweep' :: SXFunction -> (DMatrix, DMatrix) -> [BacksweepOutput] -> [BacksweepOutput]
-- end step - Vnext is 0 so q fcn is cost function
backSweep' qFunction (x,u) [] = [backPropagate qFunction x u (Quad vxx vx v0 x0)]
  where
    x0 =  zeros $ size x
    vxx = zeros (rows x, rows x)
    vx =  zeros $ size x
    v0 =  zeros (1,1)
-- all non-end steps
backSweep' qFunction (x,u) acc@((v,_,_):_) = (backPropagate qFunction x u v):acc


-- convert lists to matrix/vectors, apply the q function back propogation math, convert back to lists
backPropagate :: SXFunction -> DMatrix -> DMatrix -> Quad DMatrix -> BacksweepOutput
backPropagate qFunction x u nextValue = (Quad vxx vx v0 x, feedbackGains, openLoopControl)
  where
    -- q functions to lists to matrices/vectors
    (q0, qx, qu, qxx, qxu, quu) = evalQFunction qFunction (x,u,nextValue)

    -- value function update
    vxx' =  qxx - (qxu * (inv quu) * (trans qxu))
    vxx = scale 0.5 (vxx' + (trans vxx'))
    vx  = (trans qx) - ((trans qu) * (inv quu) * (trans qxu))
    v0  = q0- ((trans qu)*((inv quu) * qu))
    
    -- feedback gain
    feedbackGains =  -(inv quu) * (trans qxu)
    
    -- open loop control
    openLoopControl = u - ((inv quu) * qu)
