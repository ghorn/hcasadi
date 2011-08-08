-- Ddp.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Ddp(ddp, q0, qx, qu, qxx, quu, qxu) where

import Hom
import Numeric.AD
import Numeric.LinearAlgebra
import Data.List(mapAccumL)

type BacksweepOutput a = (Quad a, [[a]], [a])

----------------------- convenience function -----------------------
-- iterate ddp'
ddp :: forall a. (Floating a, Field a, Num (Vector a)) =>
          (forall s b. (Floating b, Mode s) => Cost (AD s b))
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> [State a] -> [Action a] -> [([State a], [Action a], [BacksweepOutput a])]
ddp cost dode xTraj0 uTraj0 = iterate f x0
  where
    f (xTraj, uTraj, _) = ddp' cost dode xTraj uTraj
    x0 = ddp' cost dode xTraj0 uTraj0


-- just one backsweep then forwardsweep
ddp' :: forall a. (Floating a, Field a, Num (Vector a)) =>
          (forall s b. (Floating b, Mode s) => Cost (AD s b))
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> [State a] -> [Action a] -> ([State a], [Action a], [BacksweepOutput a])
ddp' cost dode xTraj0 uTraj0 = (xTraj, uTraj, backsweepTrajectory)
  where
    backsweepTrajectory :: [BacksweepOutput a]
    backsweepTrajectory = backSweep cost dode xTraj0 uTraj0

    dode' :: Ode a
    dode' x u = map unprobe $ dode (map lift x) (map lift u)

    forwardsweepTrajectory :: [(State a, Action a)]
    forwardsweepTrajectory = forwardSweep dode' (head xTraj0) backsweepTrajectory
    (xTraj, uTraj) = unzip forwardsweepTrajectory



-------------------------- forward sweep -----------------------------
forwardSweep :: (Floating a) => Ode a -> State a -> [BacksweepOutput a]
                -> [(State a, Action a)]
forwardSweep dode x0 backsweepTrajectory = snd $ mapAccumL (fSweep dode) x0 backsweepTrajectory


fSweep :: Floating a => Ode a -> State a -> BacksweepOutput a -> (State a, (State a, Action a))
fSweep dode x (Quad _ _ _ x0, feedbackGain, uOpenLoop) = (xNext, (x, u))
  where
    u = mimoController x x0 feedbackGain uOpenLoop
    xNext = dode x u

mimoController :: Floating a => State a -> State a -> [[a]] -> Action a -> Action a
mimoController x x0 feedbackMatrix uOpenLoop = u
  where
    u = zipWith (+) uOpenLoop uClosedLoop
    uClosedLoop = mvMult feedbackMatrix deltaX
      where
        deltaX = zipWith (-) x x0
        mvMult m v = map (\a -> dotLists a v) m
          where
            dotLists xa xb = sum (zipWith (*) xa xb)




-------------------------- backward sweep --------------------------
backSweep :: (Floating a, Field a, Num (Vector a)) =>
             (forall s b. (Floating b, Mode s) => Cost (AD s b))
             -> (forall s b. (Floating b, Mode s) => Ode (AD s b)) 
             -> [State a] -> [Action a] -> [BacksweepOutput a]
backSweep cost dode xTraj0 uTraj0 = foldr (\x acc -> backSweep' cost dode x acc) [] (zip xTraj0 uTraj0)

backSweep' :: (Floating a, Field a, Num (Vector a)) =>
              (forall s b. (Floating b, Mode s) => Cost (AD s b))
              -> (forall s b. (Floating b, Mode s) => Ode (AD s b)) 
              -> (State a, Action a) -> [(Quad a, [[a]], [a])] -> [BacksweepOutput a]
-- end step - Vnext is 0 so q fcn is cost function
backSweep' cost dode (x,u) [] = [(backPropagate cost dode) x u (Quad vxx vx v0 x0)]
  where
    x0 = vx
    vxx = replicate (length x) vx
    vx = replicate (length x) v0
    v0 = 0
-- all non-end steps
backSweep' cost dode (x,u) acc@((v,_,_):_) = ((backPropagate cost dode) x u v):acc


---- back propogate value fcn one step
backPropagate :: (Floating a, Field a, Num (Vector a)) => 
                 (forall s b. (Floating b, Mode s) => Cost (AD s b))
                 -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
                 -> State a -> Action a -> Quad a -> BacksweepOutput a
backPropagate cost dode x u (Quad vxx' vx' v0' x0') = (value, feedbackGains, openLoopControl)
  where
    qx'  = qx  cost dode x u (Quad vxx' vx' v0' x0')
    qu'  = qu  cost dode x u (Quad vxx' vx' v0' x0')
    qxx' = qxx cost dode x u (Quad vxx' vx' v0' x0')
    quu' = quu cost dode x u (Quad vxx' vx' v0' x0')
    qxu' = qxu cost dode x u (Quad vxx' vx' v0' x0')
    q0'  = unprobe $ q0 cost dode (map lift x) (map lift u) (Quad liftVxx' liftVx' liftV0' liftX0')
      where
        liftVxx' = map (map lift) vxx'
        liftVx'  = map lift vx'
        liftV0'  = lift v0'
        liftX0'  = map lift x0'

    (value, feedbackGains, openLoopControl) = backPropagate' q0' qx' qu' qxx' quu' qxu' x u

-- convert lists to matrix/vectors, apply the q function back propogation math, convert back to lists
backPropagate' :: (Field a, Num (Vector a)) => a -> [a] -> [a] -> [[a]] -> [[a]] -> [[a]]
                  -> State a -> Action a -> BacksweepOutput a
backPropagate' q0' qx'' qu'' qxx'' quu'' qxu'' x0 u0 = (Quad vxx vx v0 x0, feedbackGains, openLoopControl)
  where
    -- q functions lists to matrices/vectors
    qxx' = fromLists qxx''
    quu' = fromLists quu''
    qxu' = fromLists qxu''
    qx' = fromList qx''
    qu' = fromList qu''
    
    -- value function
    vxx = toLists $ qxx' - (qxu' <> (inv quu') <> (trans qxu'))
    vx = toList $ qx' - ( qu' <> (inv quu') <> (trans qxu'))
    v0 = q0' - (qu' `dot` ((inv quu') <> qu'))
    
    -- feedback gain
    feedbackGains = toLists $ - ( inv quu') <> (trans qxu');
    
    -- open loop control
    openLoopControl = zipWith (+) u0 $ toList $ - (inv quu') <> qu';


-------- quadratic expansion of q function (unmaximized value function) -------
q0 :: Floating a => Cost a -> Ode a -> (State a -> Action a -> Quad a -> a)
q0 cost dode = \x u v -> (cost x u) + (nextValue x u v)
  where
    nextValue x u (Quad vxx vx v0 x0) = evalQuad (Quad vxx vx v0 x0) (dode x u)

qx,qu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => Cost (AD s b))
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [a]
qx cost dode x u (Quad vxx vx v0 x0) = grad g x
  where
    g x' = q' x' (map lift u) (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q0 cost dode

qu cost dode x u (Quad vxx vx v0 x0) = grad g u
  where
    g u' = q' (map lift x) u' (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q0 cost dode

qxx,qxu,quu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => Cost (AD s b))
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [[a]]
qxx cost dode x u (Quad vxx vx v0 x0) = hessian g x
  where
    g x' = q' x' (map lift u) (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q0 cost dode

quu cost dode x u (Quad vxx vx v0 x0) = hessian g u
  where
    g u' = q' (map lift x) u' (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q0 cost dode

qxu cost dode x u (Quad vxx vx v0 x0) = jacobian g u
  where
    g u' = (qx cost dode) (map lift x) u' (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0
