-- Ddp.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Ddp(backPropagate, q0, qx, qu, qxx, quu, qxu) where

import Hom
import Numeric.AD
import Numeric.LinearAlgebra

---- ddp backward propogation
backPropagate :: (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
                 -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
                 -> State Double -> Action Double -> Quad Double -> (Quad Double, [[Double]], [Double])
-- more general type (commented out cause it's more confusing to the user)                 
-- backPropagate :: forall a. (Floating a, Field a, Num (Vector a)) => 
--                  (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
--                  -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
--                  -> State a -> Action a -> Quad a -> (Quad a, [[a]], [a])
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

backPropagate' :: (Field a, Num (Vector a)) => a -> [a] -> [a] -> [[a]] -> [[a]] -> [[a]] -> State a -> Action a
                  -> (Quad a, [[a]], [a])
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
q0 :: Floating a => (State a -> Action a -> a) -> Ode a -> (State a -> Action a -> Quad a -> a)
q0 cost dode = \x u v -> (cost x u) + (nextValue x u v)
  where
    nextValue x u (Quad vxx vx v0 x0) = evalQuad (Quad vxx vx v0 x0) (dode x u)

qx :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [a]
qu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [a]
qxx :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [[a]]
quu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [[a]]

qxu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => Ode (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [[a]]

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
