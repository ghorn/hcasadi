{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.Bindings.Math ( -- * unary
                              c_sxmNegate
                            , c_sxmAbs
                            , c_sxmExp
                            , c_sxmSqrt
                            , c_sxmLog
                            , c_sxmSin
                            , c_sxmCos
                            , c_sxmTan
                            , c_sxmArcsin
                            , c_sxmArccos
                            , c_sxmArctan
                              -- * binary
                            , c_sxmPlus
                            , c_sxmMinus
                            , c_sxmTimes
                            , c_sxmDivide
                            , c_sxmPow
                              -- * matrix
                            , c_sxmMM
                            , c_sxmTranspose
                            , c_sxmInv
                              -- * AD
                            , c_sxmGradient
                            , c_sxmHessian
                            , c_sxmJacobian
                            ) where

import Casadi.Bindings.SXM ( SXMRaw )
import Foreign.Ptr ( Ptr )

--------------------- binary -------------------
foreign import ccall unsafe "sxmMath.hpp sxmPlus"   c_sxmPlus :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmMinus"  c_sxmMinus :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmTimes"  c_sxmTimes :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmDivide" c_sxmDivide :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmPow"    c_sxmPow    :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)

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

--------------------- matrix -------------------
foreign import ccall unsafe "sxmMath.hpp sxmMM"        c_sxmMM :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmTranspose" c_sxmTranspose :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmInv"       c_sxmInv       :: Ptr SXMRaw -> IO (Ptr SXMRaw)

--------------------- AD -------------------
foreign import ccall unsafe "sxmMath.hpp sxmGradient" c_sxmGradient :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmHessian"  c_sxmHessian  :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmMath.hpp sxmJacobian" c_sxmJacobian :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
