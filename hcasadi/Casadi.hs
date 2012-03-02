-- Casadi.hs

{-# OPTIONS_GHC -Wall #-}

module Casadi( module Casadi.Symbolic
             , module Casadi.Eval
             , test
             ) where

import Casadi.Symbolic
import Casadi.Eval(evalToSX)

test :: IO ()
test = do
  let x = sym "x"
      y = sym "y"
      z = x*y + cos(x/y)
  print (x,y,z)
  
  x' <- evalToSX x
  y' <- evalToSX y
  z' <- evalToSX z
  print (x',y',z')
  
  let f = replicate 100 z
  f' <- evalToSX $ sum f
  print f'
