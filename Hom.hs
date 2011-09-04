-- Hom.hs

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Hom (State, Action, Ode, Cost,
--            dA, dB,
            Quad(..), evalQuad) where

type State a = [a]
type Action a = [a]
type Ode a = State a -> Action a -> State a
type Cost a = State a -> Action a -> a


---------- linearize dynamics f ~= f0 + dA*(x-x0) + dB*(u - u0) ---------
--dA :: Floating a => Ode a -> [[a]]
--dA f x u = jacobian g x
--  where
--    g x' = f x' (map lift u)
--
--
--dB :: Floating a => (forall s b. (Floating b, Mode s) =>
--                     [AD s b] -> [AD s b] -> [AD s b]) -> (State a -> Action a -> [[a]])
--dB f x u = jacobian g u
--  where
--    g u' = f (map lift x) u'


------------- quadratic data type -----------
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
