-- Hom.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Hom (State, Action, Ode, eulerStep, rk4Step, dA, dB,
            cx, cu, cxx, cuu, cxu,
            qx, qu, qxx, quu, qxu, Quad(..), evalQuad) where

import Numeric.AD

type State a = [a]
type Action a = [a]
type Ode a = State a -> Action a -> State a

---------------- general stuff -----------------
eulerStep :: (Floating a) => Ode a -> State a -> Action a -> a -> State a
eulerStep dxdt x u dt = map (*dt) (dxdt x u)

rk4Step :: (Floating a) => Ode a -> State a -> Action a -> a -> State a
rk4Step dxdt x u dt = map (/6) $ addLists [k1, twok2, twok3, k4]
  where
    addLists [] = []
    addLists (a:as) = foldl (\acc y -> zipWith (+) acc y) a as
    
    k1 = map (*dt) $ dxdt x u
    k2 = map (*dt) $ addLists [x, map (*0.5) k1]
    k3 = map (*dt) $ addLists [x, map (*0.5) k2]
    k4 = map (*dt) $ addLists [x, k3]
    
    twok2 = map (*2) k2
    twok3 = map (*2) k3

-- linearize dynamics f ~= f0 + dA*(x-x0) + dB*(u - u0)
dA :: Floating a => (forall s b. (Floating b, Mode s) =>
                     [AD s b] -> [AD s b] -> [AD s b]) -> (State a -> Action a -> [[a]])
dA f x u = jacobian g x
  where
    g x' = f x' (map lift u)


dB :: Floating a => (forall s b. (Floating b, Mode s) =>
                     [AD s b] -> [AD s b] -> [AD s b]) -> (State a -> Action a -> [[a]])
dB f x u = jacobian g u
  where
    g u' = f (map lift x) u'


---------- quadratic expansion of cost(x,u) ----------
cx :: Floating a => (forall s b. (Floating b, Mode s) =>
                     [AD s b] -> [AD s b] -> AD s b) -> (State a -> Action a -> [a])
cu :: Floating a => (forall s b. (Floating b, Mode s) =>
                     [AD s b] -> [AD s b] -> AD s b) -> (State a -> Action a -> [a])
cxx :: Floating a => (forall s b. (Floating b, Mode s) =>
                      [AD s b] -> [AD s b] -> AD s b) -> (State a -> Action a -> [[a]])
cuu :: Floating a => (forall s b. (Floating b, Mode s) =>
                      [AD s b] -> [AD s b] -> AD s b) -> (State a -> Action a -> [[a]])
cxu :: Floating a => (forall s b. (Floating b, Mode s) =>
                      [AD s b] -> [AD s b] -> AD s b) -> (State a -> Action a -> [[a]])


cx cost x u = grad g x
  where
    g x' = cost x' (map lift u)

cu cost x u = grad g u
  where
    g u' = cost (map lift x) u'

cxx cost x u = hessian g x
  where
    g x' = cost x' (map lift u)

cuu cost x u = hessian g u
  where
    g u' = cost (map lift x) u'

cxu cost x u = jacobian g u
  where
    g u' = (cx cost) (map lift x) u'


-------- quadratic expansion of q function (unmaximized value function) -------
qx :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> State (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [a]
qu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> State (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [a]
qxx :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> State (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [[a]]
quu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> State (AD s b))
       -> State a
       -> Action a
       -> Quad a
       -> [[a]]

qxu :: forall a. Floating a =>
       (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> AD s b)
       -> (forall s b. (Floating b, Mode s) => State (AD s b) -> Action (AD s b) -> State (AD s b))
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
    q' = q cost dode

qu cost dode x u (Quad vxx vx v0 x0) = grad g u
  where
    g u' = q' (map lift x) u' (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q cost dode

qxx cost dode x u (Quad vxx vx v0 x0) = hessian g x
  where
    g x' = q' x' (map lift u) (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q cost dode

quu cost dode x u (Quad vxx vx v0 x0) = hessian g u
  where
    g u' = q' (map lift x) u' (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

    q' :: forall s. Mode s => State (AD s a) -> Action (AD s a) -> Quad (AD s a) -> AD s a
    q' = q cost dode

qxu cost dode x u (Quad vxx vx v0 x0) = jacobian g u
  where
    g u' = (qx cost dode) (map lift x) u' (Quad vxx' vx' v0' x0')
      where
        vxx' = map (map lift) vxx
        vx' = map lift vx
        v0' = lift v0
        x0' = map lift x0

q :: Floating a => (State a -> Action a -> a) -> Ode a -> (State a -> Action a -> Quad a -> a)
q cost dode = \x u v -> (cost x u) + (nextValue x u v)
  where
    nextValue x u (Quad vxx vx v0 x0) = evalQuad (Quad vxx vx v0 x0) (dode x u)


data Quad a = Quad [[a]] [a] a [a] deriving (Show)

evalQuad :: Floating a => Quad a -> [a] -> a
evalQuad (Quad h g a x0) x = constTerm + linearTerm + quadraticTerm
   where
     constTerm = a
     linearTerm = dotLists g x' -- put length check here
     quadraticTerm = (*0.5) $ dotLists x' $ map (\y -> dotLists y x') h -- put length check here

     dotLists :: Floating a => [a] -> [a] -> a
     dotLists xa xb = sum (zipWith (*) xa xb)

     x' = zipWith (-) x x0
