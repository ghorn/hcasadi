{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.Bindings.Math ( -- * unary
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

import Casadi.Bindings.SXM
import Casadi.Bindings.CasadiInterfaceUtils ( withForeignPtrs2 )

import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Control.Exception(mask_)

sxmWrapBinary :: (Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)) -> SXM -> SXM -> IO SXM
sxmWrapBinary c_fun (SXM sxm0Raw) (SXM sxm1Raw) = mask_ $ do
  smxOutRaw <- withForeignPtrs2 c_fun sxm0Raw sxm1Raw >>= newForeignPtr c_sxmDelete
  return $ SXM smxOutRaw

sxmWrapUnary :: (Ptr SXMRaw -> IO (Ptr SXMRaw)) -> SXM -> IO SXM
sxmWrapUnary c_fun (SXM sxmInRaw) = mask_ $ do
  smxOutRaw <- withForeignPtr sxmInRaw c_fun >>= newForeignPtr c_sxmDelete
  return $ SXM smxOutRaw

--------------------- binary -------------------
foreign import ccall unsafe "sxmMath.hpp sxmPlus"   c_sxmPlus :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmMinus"  c_sxmMinus :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmTimes"  c_sxmTimes :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmDivide" c_sxmDivide :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmPow"    c_sxmPow    :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)

sxmPlus, sxmMinus, sxmTimes, sxmDivide, sxmPow :: SXM -> SXM -> IO SXM
sxmPlus   = sxmWrapBinary c_sxmPlus
sxmMinus  = sxmWrapBinary c_sxmMinus
sxmTimes  = sxmWrapBinary c_sxmTimes
sxmDivide = sxmWrapBinary c_sxmDivide
sxmPow    = sxmWrapBinary c_sxmPow

---------------------- unary ----------------------
foreign import ccall unsafe "sxmMath.hpp sxmNegate" c_sxmNegate :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmAbs"    c_sxmAbs    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmExp"    c_sxmExp    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmSqrt"   c_sxmSqrt   :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmLog"    c_sxmLog    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmSin"    c_sxmSin    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmCos"    c_sxmCos    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmTan"    c_sxmTan    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmArcsin" c_sxmArcsin :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmArccos" c_sxmArccos :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmArctan" c_sxmArctan :: Ptr SXMRaw -> IO (Ptr SXMRaw)

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
foreign import ccall unsafe "sxmMM"        c_sxmMM :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmTranspose" c_sxmTranspose :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmInv"       c_sxmInv       :: Ptr SXMRaw -> IO (Ptr SXMRaw)

sxmMM :: SXM -> SXM -> IO SXM
sxmMM = sxmWrapBinary c_sxmMM

sxmTranspose, sxmInv :: SXM -> IO SXM
sxmTranspose = sxmWrapUnary c_sxmTranspose
sxmInv       = sxmWrapUnary c_sxmInv
