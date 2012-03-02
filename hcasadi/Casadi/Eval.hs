{-# OPTIONS_GHC -Wall #-}
{-# Language GADTs #-}

module Casadi.Eval( evalToSX
                  ) where

import Python.Exceptions

import qualified Casadi.Bindings as C
import Casadi.Bindings(CasadiModule, SX)
import Casadi.Symbolic(Expr(..))
import Casadi.BinUn(casadiApplyUnary, casadiApplyBinary, BinOp(Mul))

evalToSX :: Expr a -> IO SX
evalToSX x = handlePy exc2ioerror $ do
  casadi <- C.casadiInit
  eval casadi x

eval :: CasadiModule -> Expr a -> IO SX
eval casadi (ESym _ name) = C.sym casadi name
eval casadi (EConst x)    = C.sxDouble casadi x
eval casadi (EInt k)      = C.sxInt casadi k
eval casadi (EUnary unOp x) = do
  x' <- eval casadi x
  casadiApplyUnary unOp casadi x'
eval casadi (EBinary binOp x y) = do
  x' <- eval casadi x
  y' <- eval casadi y
  casadiApplyBinary binOp casadi x' y'
eval casadi (EScale x y) = do
  x' <- eval casadi x
  y' <- eval casadi y
  casadiApplyBinary Mul casadi x' y'
eval casadi (EDeriv ex arg) = do
  ex' <- eval casadi ex
  arg' <- eval casadi arg
  C.gradient casadi ex' arg'
eval casadi (EGrad ex arg) = do
  ex' <- eval casadi ex
  arg' <- eval casadi arg
  C.gradient casadi ex' arg'
eval casadi (EJacob ex arg) = do
  ex' <- eval casadi ex
  arg' <- eval casadi arg
  C.jacobian casadi ex' arg'
eval casadi (EHess ex arg) = do
  ex' <- eval casadi ex
  arg' <- eval casadi arg
  C.hessian casadi ex' arg'
eval casadi (EDot x y) = do
  x' <- eval casadi x
  y' <- eval casadi y
  C.matrixMultiply casadi x' y'
