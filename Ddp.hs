-- Ddp.hs

{-# OPTIONS_GHC -Wall #-}

module Ddp
       (
         prepareDdp,
         BacksweepOutput
       ) where

import Casadi
import Casadi.SXFunction
import Hom(Cost, Ode, Quad(..), evalQuad)
import Data.List(mapAccumL)


data BacksweepOutput a = BacksweepOutput { valueQuad'    :: Quad a
                                         , feedbackGain' :: a
                                         , uOpenLoop'    :: a
                                         } deriving Show

data DdpTrajectory a = DdpTrajectory [a] [a] [BacksweepOutput a] deriving Show
type QFunction a = (a,a,a,Quad a) -> (a,a,a,a,a,a)

prepareQFunction :: Matrix a b c => (Int, Int) -> Cost SXMatrix -> Ode SXMatrix -> QFunction a
prepareQFunction (nx, nu) costFunction dode = qFunctionRet
  where
    alpha = sxMatrixSymbolic "qFun_alpha"   (1,1)

    x   = sxMatrixSymbolic "qFun_x"   (nx,1)
    u   = sxMatrixSymbolic "qFun_u"   (nu,1)

    vxx = sxMatrixSymbolic "qFun_vxx" (nx,nx)
    vx  = sxMatrixSymbolic "qFun_vx"  (nx,1)
    v0  = sxMatrixSymbolic "qFun_v0"  (1,1)
    x0  = sxMatrixSymbolic "qFun_x0"  (nx,1)
    
    -- q function and its quadratic expansion
    q = ((costFunction x u) + (evalQuad nextValue (dode x u)))*(1-alpha) + (xError + uError)*alpha
      where
        xError = (trans(x))*x
        uError = (trans(u))*u
        nextValue = Quad vxx vx v0 x0
    qx = gradient q x
    qu = gradient q u
    qxx = hessian q x
    quu = hessian q u
    qxu = jacobian qx u
    
    qSXFunction = sxFunction [alpha,x,u,vxx,vx,v0,x0] [q,qx,qu,qxx,qxu,quu]
    
    qFunctionRet (alpha', x', u', Quad vxx' vx' v0' x0') = (q', qx', qu', qxx', qxu', quu')
      where
        [q',qx',qu',qxx',qxu',quu'] = qSXFunction [alpha',x',u',vxx',vx',v0',x0']

----------------------- convenience function -----------------------
-- prepare ddp  
prepareDdp :: String -> Cost SXMatrix -> Ode SXMatrix -> (Int, Int) -> Int -> (SXMatrix, SXMatrix)
              -> ( DMatrix -> [DMatrix] -> [DMatrix] -> [([DMatrix], [DMatrix])] )
--              -> (  [DMatrix] -> [DMatrix] -> [DdpTrajectory DMatrix] )
--              -> ([DMatrix] -> [DMatrix] -> [([DMatrix], [DMatrix], [BacksweepOutput a])])
prepareDdp name cost dode (nx,nu) nSteps uBounds = iterateDdp
  where
    xTraj0  = map (\k -> sxMatrixSymbolic ("xTraj_"++show k) (nx,1)) [0..nSteps-1]
    uTraj0  = map (\k -> sxMatrixSymbolic ("uTraj_"++show k) (nu,1)) [0..nSteps-1]
    alpha  = sxMatrixSymbolic "alpha" (1,1)

    DdpTrajectory xTrajNew uTrajNew bsOutput = ddp (prepareQFunction (nx,nu) cost dode) alpha dode uBounds xTraj0 uTraj0
    
    ddpSXFun :: SXFunction
    ddpSXFun = sxFunctionCreate (alpha:xTraj0 ++ uTraj0) (xTrajNew++uTrajNew)
    
    ddpFun :: [DMatrix] -> [DMatrix]
    --ddpFun = sxFunctionEvaluate ddpSXFun
    ddpFun = sxFunctionCompile ddpSXFun name
    
    oneDdp :: DMatrix -> [DMatrix] -> [DMatrix] -> ([DMatrix], [DMatrix])
    oneDdp alpha' xTraj uTraj = (xOut,uOut)
      where
        xuOut = ddpFun (alpha':xTraj ++ uTraj)
        (xOut,uOut) = splitAt nSteps xuOut

    iterateDdp alpha' xTraj0' uTraj0' = iterate (\(xTraj, uTraj) -> oneDdp alpha' xTraj uTraj) (oneDdp alpha' xTraj0' uTraj0')


-- just one backsweep then forwardsweep
ddp :: Matrix a b c => QFunction a -> a -> Ode a -> (a,a) -> [a] -> [a] -> DdpTrajectory a
ddp qFun alpha dode uBounds xTraj0 uTraj0 = DdpTrajectory xTraj uTraj backsweepTrajectory
  where
    backsweepTrajectory = backSweep qFun alpha xTraj0 uTraj0

    forwardsweepTrajectory = forwardSweep dode (head xTraj0) backsweepTrajectory uBounds
    (xTraj, uTraj) = unzip forwardsweepTrajectory


-------------------------- forward sweep -----------------------------
forwardSweep :: Matrix a b c => Ode a -> a -> [BacksweepOutput a] -> (a,a) -> [(a, a)]
forwardSweep dode x0 backsweepTrajectory uBounds = snd $ mapAccumL (fSweep uBounds dode) x0 backsweepTrajectory


fSweep :: Matrix a b c => (a,a) -> Ode a -> a -> BacksweepOutput a -> (a, (a, a))
fSweep uBounds dode x backsweepOutput = (xNext, (x, u))
  where
    (Quad _ _ _ x0) = valueQuad' backsweepOutput
    feedbackGain = feedbackGain' backsweepOutput
    uOpenLoop = uOpenLoop' backsweepOutput

    u = mimoController uBounds x x0 feedbackGain uOpenLoop
    xNext = dode x u


mimoController :: Matrix a b c => (a,a) -> a -> a -> a -> a -> a
mimoController uBounds x x0 feedbackMatrix uOpenLoop = u
  where
    u' = uOpenLoop + feedbackMatrix*(x - x0)
    u = bound u' uBounds


-------------------------- backward sweep --------------------------
backSweep :: Matrix a b c => QFunction a -> a -> [a] -> [a] -> [BacksweepOutput a]
backSweep qFun alpha xTraj0 uTraj0 = foldr (\x acc -> backSweep' qFun alpha x acc) [] (zip xTraj0 uTraj0)

backSweep' :: Matrix a b c => QFunction a -> a -> (a, a) -> [BacksweepOutput a] -> [BacksweepOutput a]
-- end step - Vnext is 0 so q fcn is cost function
backSweep' qFun alpha (x,u) [] = [backPropagate qFun alpha x u (Quad vxx vx v0 x0)]
  where
    x0 =  zeros $ size x
    vxx = zeros (rows x, rows x)
    vx =  zeros $ size x
    v0 =  zeros (1,1)
-- all non-end steps
backSweep' qFun alpha (x,u) acc@(bsOut:_) = (backPropagate qFun alpha x u (valueQuad' bsOut)):acc


-- convert lists to matrix/vectors, apply the q function back propagation math, convert back to lists
backPropagate :: Matrix a b c => QFunction a -> a -> a -> a -> Quad a -> BacksweepOutput a
backPropagate qFun alpha x u nextValue = BacksweepOutput { valueQuad'    = Quad vxx vx v0 x
                                                         , feedbackGain' = feedbackGains
                                                         , uOpenLoop'    = openLoopControl
                                                         }
  where

    -- q functions to lists to matrices/vectors
    (q0, qx, qu, qxx, qxu, quu) = qFun (alpha,x,u,nextValue)

    -- value function update
    vxx =  balanceSymm $ qxx - qxu*(inv quu)*(trans qxu)
      where
        balanceSymm m = scale (0.5) (m + (trans m))
    vx  = qx - qxu*(inv(quu))*qu
    v0  = q0 - (trans qu)*(inv quu)*qu
    
    -- feedback gain
    feedbackGains =  -(inv quu)*(trans qxu)
    
    -- open loop control
    openLoopControl = u - (inv quu)*qu
