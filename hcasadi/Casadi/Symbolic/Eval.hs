{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language GADTs #-}
{-# Language ConstraintKinds #-}
--{-# Language FlexibleContexts #-}
--{-# Language TypeSynonymInstances #-}
--{-# Language FlexibleInstances #-}
-- {-# MultiParamTypeClasses, FlexibleInstances #-}

module Casadi.Symbolic.Eval( eval
                           , woohoo
                           ) where

import Control.Monad(liftM)

import Casadi.Bindings.SXM(sym, sxNewDouble, sxNewIntegral, SXM(..))
import Casadi.Symbolic.Symbolic(Expr(..))
import Casadi.Symbolic.BinUn(casadiApplyUnary, casadiApplyBinary)

data Cas a = Cas SXM

eval :: Expr a -> IO (Cas a)
eval (ESym _ name) = liftM Cas (sym name)
eval (EConst x) = liftM Cas (sxNewDouble x)
eval (EInt k) = liftM Cas (sxNewIntegral k)
eval (EUnary unOp x) = do
  Cas x' <- eval x
  z <- casadiApplyUnary unOp x'
  return (Cas z)
eval (EBinary binOp x y) = do
  Cas x' <- eval x
  Cas y' <- eval y
  z <- casadiApplyBinary binOp x' y'
  return (Cas z)
--eval (EScale s x) = 
--eval EDot :: (Dot b c ~ a) => Expr b -> Expr c -> Expr a
--eval EDeriv :: (a ~ DIM0) => Expr DIM0 -> Expr DIM0 -> Expr a
--eval EGrad  :: (a ~ DIM1) => Expr DIM0 -> Expr DIM1 -> Expr a
--eval EHess  :: (a ~ DIM2) => Expr DIM0 -> Expr DIM1 -> Expr a
--eval EJacob :: (a ~ DIM2) => Expr DIM1 -> Expr DIM1 -> Expr a

woohoo = do
  x <- sym "x"
  print x
