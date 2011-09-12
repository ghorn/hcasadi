-- Ddp.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Ddp
       (
         prepareDdp,
         BacksweepOutput
       ) where

import Casadi
import Hom(Cost, Ode, Quad(..), evalQuad)
import Data.List(mapAccumL)

type BacksweepOutput a = (Quad a, a, a)

prepareQFunction :: Matrix a b c => (Int, Int) -> Cost SXMatrix SX -> Ode SXMatrix
                    -> ((a, a, Quad a) -> (a,a,a,a,a,a))
prepareQFunction (nx, nu) costFunction dode = qFunctionRet
  where
    x   = sxMatrixSymbolic "qFun_x"   (nx,1)
    u   = sxMatrixSymbolic "qFun_u"   (nu,1)
    vxx = sxMatrixSymbolic "qFun_vxx" (nx,nx)
    vx  = sxMatrixSymbolic "qFun_vx"  (nx,1)
    v0  = sxMatrixSymbolic "qFun_v0"  (1,1)
    x0  = sxMatrixSymbolic "qFun_x0"  (nx,1)
    
    -- q function and its quadratic expansion
    q = (costFunction x u) + (evalQuad nextValue (dode x u))
      where
        nextValue = Quad vxx vx v0 x0
    qx = gradient q x
    qu = gradient q u
    qxx = hessian q x
    quu = hessian q u
    qxu = jacobian qx u
    
    qFunctionRet :: Matrix a b c => (a, a, Quad a) -> (a,a,a,a,a,a)
    qFunctionRet (x', u', Quad vxx' vx' v0' x0') = (q', qx', qu', qxx', qxu', quu')
      where
        [q',qx',qu',qxx',qxu',quu'] = sxFunctionEvaluate qSXFunction [x',u', vxx', vx', v0', x0']
        qSXFunction = sxFunction [x,u,vxx,vx,v0,x0] [fromList[q],qx,qu,qxx,qxu,quu]

----------------------- convenience function -----------------------
-- prepare ddp  
prepareDdp :: Cost SXMatrix SX -> Ode SXMatrix -> (Int, Int) -> Int -> (SXMatrix, SXMatrix)
              -> (  [DMatrix] -> [DMatrix] -> [([DMatrix], [DMatrix])]  )
--              -> ([DMatrix] -> [DMatrix] -> [([DMatrix], [DMatrix], [BacksweepOutput a])])
prepareDdp cost dode (nx,nu) nSteps uBounds = iterateDdp
  where
    xTraj0  = map (\k -> sxMatrixSymbolic ("xTraj_"++show k) (nx,1)) [0..nSteps-1]
    uTraj0  = map (\k -> sxMatrixSymbolic ("uTraj_"++show k) (nu,1)) [0..nSteps-1]

    (xTrajNew, uTrajNew, bsOutput) = ddp (prepareQFunction (nx,nu) cost dode) dode uBounds xTraj0 uTraj0
    
    ddpSx :: SXFunction
    ddpSx = sxFunction (xTraj0 ++ uTraj0) (xTrajNew++uTrajNew)

    oneDdp :: [DMatrix] -> [DMatrix] -> ([DMatrix], [DMatrix])
    oneDdp xTraj uTraj = (xOut,uOut)
      where
        xuOut = sxFunctionEvaluate ddpSx (xTraj ++ uTraj)
        (xOut,uOut) = splitAt nSteps xuOut

    iterateDdp xTraj0' uTraj0' = iterate (\(xTraj, uTraj) -> oneDdp xTraj uTraj) (oneDdp xTraj0' uTraj0')


-- just one backsweep then forwardsweep
ddp :: Matrix a b c => ((a,a,Quad a) -> (a,a,a,a,a,a)) -> Ode a -> (a,a) -> [a] -> [a] -> ([a], [a], [BacksweepOutput a])
ddp qFun dode uBounds xTraj0 uTraj0 = (xTraj, uTraj, backsweepTrajectory)
  where
    backsweepTrajectory = backSweep qFun xTraj0 uTraj0

    forwardsweepTrajectory = forwardSweep dode (head xTraj0) backsweepTrajectory uBounds
    (xTraj, uTraj) = unzip forwardsweepTrajectory


-------------------------- forward sweep -----------------------------
forwardSweep :: Matrix a b c => Ode a -> a -> [BacksweepOutput a] -> (a,a) -> [(a, a)]
forwardSweep dode x0 backsweepTrajectory uBounds = snd $ mapAccumL (fSweep uBounds dode) x0 backsweepTrajectory


fSweep :: Matrix a b c => (a,a) -> Ode a -> a -> BacksweepOutput a -> (a, (a, a))
fSweep uBounds dode x (Quad _ _ _ x0, feedbackGain, uOpenLoop) = (xNext, (x, u))
  where
    u = mimoController uBounds x x0 feedbackGain uOpenLoop
    xNext = dode x u


mimoController :: Matrix a b c => (a,a) -> a -> a -> a -> a -> a
mimoController uBounds x x0 feedbackMatrix uOpenLoop = u
  where
    u' = uOpenLoop + feedbackMatrix*(x - x0)
    u = bound u' uBounds


-------------------------- backward sweep --------------------------
backSweep :: Matrix a b c => ((a,a,Quad a) -> (a,a,a,a,a,a)) -> [a] -> [a] -> [BacksweepOutput a]
backSweep qFun xTraj0 uTraj0 = foldr (\x acc -> backSweep' qFun x acc) [] (zip xTraj0 uTraj0)

backSweep' :: Matrix a b c => ((a,a,Quad a) -> (a,a,a,a,a,a)) -> (a, a) -> [BacksweepOutput a] -> [BacksweepOutput a]
-- end step - Vnext is 0 so q fcn is cost function
backSweep' qFun (x,u) [] = [backPropagate qFun x u (Quad vxx vx v0 x0)]
  where
    x0 =  zeros $ size x
    vxx = zeros (rows x, rows x)
    vx =  zeros $ size x
    v0 =  zeros (1,1)
-- all non-end steps
backSweep' qFun (x,u) acc@((v,_,_):_) = (backPropagate qFun x u v):acc


-- convert lists to matrix/vectors, apply the q function back propagation math, convert back to lists
backPropagate :: Matrix a b c => ((a,a,Quad a) -> (a,a,a,a,a,a)) -> a -> a -> Quad a -> BacksweepOutput a
backPropagate qFun x u nextValue = out
  where
    out = (Quad vxx vx v0 x, feedbackGains, openLoopControl)
    -- q functions to lists to matrices/vectors
    (q0, qx, qu, qxx, qxu, quu) = qFun (x,u,nextValue)

    -- value function update
    vxx' =  qxx - (qxu * (inv quu) * (trans qxu))
    vxx = scale 0.5 (vxx' + (trans vxx'))
    vx  = (trans qx) - ((trans qu) * (inv quu) * (trans qxu))
    v0  = q0- ((trans qu)*((inv quu) * qu))
    
    -- feedback gain
    feedbackGains =  -(inv quu) * (trans qxu)
    
    -- open loop control
    openLoopControl = u - ((inv quu) * qu)
