-- Ddp.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleContexts #-}
--{-# OPTIONS_GHC -Wall #-}

module Ddp where

import Hom
import Numeric.AD
import Numeric.LinearAlgebra

trajectory :: Floating a => [(State a, Action a)]
trajectory = []

-- spring ode
sDxdt :: Floating a => Ode a
sDxdt state action = [v, (-k*x - b*v + u)/mass] where
  x    = state  !! 0
  v    = state  !! 1
  u    = action !! 0
  k    = 10
  b    = 0.1
  mass = 1

-- euler step
sEuler :: Floating a => State a -> Action a -> a -> State a
sEuler x u dt = eulerStep sDxdt x u dt

-- cost fcn to test following expansions
sCost :: Floating a => State a -> Action a -> a
sCost x u = 7*position*position + 2*velocity*velocity + 4*force*force
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0


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
    q0'  = unprobe $ q0  cost dode (map lift x) (map lift u) (Quad liftVxx' liftVx' liftV0' liftX0')
      where
        liftVxx' = map (map lift) vxx'
        liftVx'  = map lift vx'
        liftV0'  = lift v0'
        liftX0'  = map lift x0'

    (value, feedbackGains, openLoopControl) = backPropagate' q0' qx' qu' qxx' quu' qxu' x

backPropagate' :: (Field a, Num (Vector a)) => a -> [a] -> [a] -> [[a]] -> [[a]] -> [[a]] -> [a]
                  -> (Quad a, [[a]], [a])
backPropagate' q0' qx'' qu'' qxx'' quu'' qxu'' x0 = (Quad vxx vx v0 x0, feedbackGains, openLoopControl)
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
    openLoopControl = toList $ - (inv quu') <> qu';


main :: IO ()
main = do let v0 = Quad [[0,0],[0,0]] [0,0] 0 [0,0]
              x0 = [0,0]
              u0 = [1]
              
              dt :: Floating a => a
              dt = 0.1

          print $ backPropagate sCost (\x u -> sEuler x u dt) x0 u0 v0
