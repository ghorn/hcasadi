{-# OPTIONS_GHC -Wall #-}

module Casadi.Math ( -- * unary
                     sxmNegate
                   , sxmAbs
                   , sxmExp
                   , sxmSqrt
                   , sxmLog
                   , sxmSin
                   , sxmCos
                   , sxmTan
                   , sxmArcsin
                   , sxmArccos
                   , sxmArctan
                     -- * binary
                   , sxmPlus
                   , sxmMinus
                   , sxmTimes
                   , sxmDivide
                   , sxmPow
                     -- * matrix
                   , sxmMM
                   , sxmTranspose
                   , sxmInv
                   ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Control.Exception(mask_)

import Casadi.Bindings.SXM
import Casadi.Bindings.Math
import Casadi.SXM
import Casadi.CasadiInterfaceUtils ( withForeignPtrs2 )

sxmWrapBinary :: (Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)) -> SXM -> SXM -> IO SXM
sxmWrapBinary c_fun (SXM sxm0Raw) (SXM sxm1Raw) = mask_ $ do
  smxOutRaw <- withForeignPtrs2 c_fun sxm0Raw sxm1Raw >>= newForeignPtr c_sxmDelete
  return $ SXM smxOutRaw

sxmWrapUnary :: (Ptr SXMRaw -> IO (Ptr SXMRaw)) -> SXM -> IO SXM
sxmWrapUnary c_fun (SXM sxmInRaw) = mask_ $ do
  smxOutRaw <- withForeignPtr sxmInRaw c_fun >>= newForeignPtr c_sxmDelete
  return $ SXM smxOutRaw

--------------------- binary -------------------
sxmPlus, sxmMinus, sxmTimes, sxmDivide, sxmPow :: SXM -> SXM -> IO SXM
sxmPlus   = sxmWrapBinary c_sxmPlus
sxmMinus  = sxmWrapBinary c_sxmMinus
sxmTimes  = sxmWrapBinary c_sxmTimes
sxmDivide = sxmWrapBinary c_sxmDivide
sxmPow    = sxmWrapBinary c_sxmPow

---------------------- unary ----------------------
sxmNegate, sxmAbs, sxmExp, sxmSqrt, sxmLog, sxmSin, sxmCos, sxmTan, sxmArcsin, sxmArccos, sxmArctan :: SXM -> IO SXM
sxmNegate = sxmWrapUnary c_sxmNegate
sxmAbs    = sxmWrapUnary c_sxmAbs
sxmExp    = sxmWrapUnary c_sxmExp
sxmSqrt   = sxmWrapUnary c_sxmSqrt
sxmLog    = sxmWrapUnary c_sxmLog
sxmSin    = sxmWrapUnary c_sxmSin
sxmCos    = sxmWrapUnary c_sxmCos
sxmTan    = sxmWrapUnary c_sxmTan
sxmArcsin = sxmWrapUnary c_sxmArcsin
sxmArccos = sxmWrapUnary c_sxmArccos
sxmArctan = sxmWrapUnary c_sxmArctan

--------------------- matrix -------------------
sxmMM :: SXM -> SXM -> IO SXM
sxmMM = sxmWrapBinary c_sxmMM

sxmTranspose, sxmInv :: SXM -> IO SXM
sxmTranspose = sxmWrapUnary c_sxmTranspose
sxmInv       = sxmWrapUnary c_sxmInv