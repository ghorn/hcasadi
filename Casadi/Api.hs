-- Api.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Casadi.Api
       (
         SX(..)
       , SXMatrix(..)
       , DMatrix(..)
       , Matrix(..)
       , sxSymbolic
       , sxMatrixSymbolic
       , sxInt
       , sxDouble
       ) where

import Casadi.SX
import Casadi.DMatrix
import Casadi.SXMatrix
import Casadi.Matrix
import System.IO.Unsafe(unsafePerformIO)

sxSymbolic :: String -> SX
sxSymbolic name = unsafePerformIO $ do
  sym <- sxCreateSymbolic name
  return sym

sxMatrixSymbolic :: String -> (Int,Int) -> SXMatrix
sxMatrixSymbolic prefix dim' = unsafePerformIO $ do
  mat <- sxMatrixCreateSymbolic prefix dim'
  return mat

sxInt :: Int -> SX
sxInt = sxFromInt

sxDouble :: Double -> SX
sxDouble = sxFromDouble

--sxIntegral :: Integral a => a -> SX
--sxIntegral = sxFromIntegral

