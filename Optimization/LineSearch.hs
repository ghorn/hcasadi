-- LineSearch.hs

{-# OPTIONS_GHC -Wall #-}

module Optimization.LineSearch
       (
         goldenSectionSearch
       ) where

tau :: Floating a => a
tau = 2/(1+sqrt(5))

goldenSectionSearch :: (Ord a, Floating b) => (b -> a) -> (b, b) -> [(b,a)]
goldenSectionSearch f (x0,x3) = gss f (x0,x1,x2,x3)
  where
    x1 = x0 + (x3-x0)*(1-tau)
    x2 = x0 + (x3-x0)*tau

gss :: (Ord a, Floating b) => (b -> a) -> (b, b, b, b) -> [(b,a)]
gss f (x0, x1, x2, x3) = xs
  where
    x1' = x0 + (x2-x0)*(1-tau)
    x2' = x1 + (x3-x1)*tau
    xs
      | (f x1) < (f x2) = (x1, f x1):(gss f (x0,x1',x1,x2))
      | otherwise       = (x2, f x2):(gss f (x1,x2,x2',x3))
