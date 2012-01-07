-- SX.hs

{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-cse -fno-full-laziness #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.SX
       (
         SX(..)
       , SXRaw(..)
       , sxNewDouble
       , sxNewInt
       , sxNewIntegral
       , sxFromInt
       , sxFromIntegral
       , sxFromDouble
       , sxCreateSymbolic
       , sxBound
       ) where

import Casadi.CasadiInterfaceUtils
import Casadi.Matrix

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq
  --import Data.Ratio(numerator, denominator)

-- the SX data type
data SXRaw = SXRaw
newtype SX = SX (ForeignPtr SXRaw)

instance NFData SX where
  rnf x = x `seq` ()

-- foreign imports
foreign import ccall unsafe "sxInterface.hpp sxCreateSymbolic" c_sxCreateSymbolic :: Ptr CChar -> IO (Ptr SXRaw)
foreign import ccall unsafe "sxInterface.hpp sxNewDouble" c_sxNewDouble :: CDouble -> IO (Ptr SXRaw)
foreign import ccall unsafe "sxInterface.hpp sxNewInt"    c_sxNewInt :: CInt -> IO (Ptr SXRaw)
foreign import ccall unsafe "sxInterface.hpp &sxDelete"   c_sxDelete :: FunPtr (Ptr SXRaw -> IO ())
foreign import ccall unsafe "sxInterface.hpp sxShow"      c_sxShow :: Ptr CChar -> CInt -> (Ptr SXRaw) -> IO ()

foreign import ccall unsafe "sxInterface.hpp sxEqual"  c_sxEqual :: Ptr SXRaw -> Ptr SXRaw -> IO CInt
foreign import ccall unsafe "sxInterface.hpp sxPlus"   c_sxPlus :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxMinus"  c_sxMinus :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxTimes"  c_sxTimes :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxDivide" c_sxDivide :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxNegate" c_sxNegate :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxAbs"    c_sxAbs :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxSignum" c_sxSignum :: Ptr SXRaw -> IO CInt

foreign import ccall unsafe "sxInterface.hpp sxPi"     c_sxPi     :: Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxExp"    c_sxExp    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxSqrt"   c_sxSqrt   :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxLog"    c_sxLog    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxPow"    c_sxPow    :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxSin"    c_sxSin    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxCos"    c_sxCos    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxTan"    c_sxTan    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxArcsin" c_sxArcsin :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxArccos" c_sxArccos :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "sxInterface.hpp sxArctan" c_sxArctan :: Ptr SXRaw -> Ptr SXRaw -> IO ()

foreign import ccall unsafe "sxInterface.hpp sxBound" c_sxBound :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()

-- creation
sxCreateSymbolic :: String -> IO SX
sxCreateSymbolic name = mask_ $ do
  cName <- newCString name
  sym <- c_sxCreateSymbolic cName >>= newForeignPtr c_sxDelete
  return $ SX sym

sxNewDouble :: Double -> IO SX
sxNewDouble val = mask_ $ do
    f <- c_sxNewDouble (realToFrac val) >>= newForeignPtr c_sxDelete
    return $ SX f

sxNewInt :: Int -> IO SX
sxNewInt val = mask_ $ do
    f <- c_sxNewInt (fromIntegral val) >>= newForeignPtr c_sxDelete
    return $ SX f


sxNewIntegral :: Integral a => a -> IO SX
sxNewIntegral val
      | withinCIntBounds val = sxNewInt (fromIntegral val)
      | otherwise            = error "input out of range of CInt in sxNewIntegral"
        where
            withinCIntBounds x = and [fromIntegral x <= maxCInt, fromIntegral x >= minCInt]
            maxCInt = toInteger (maxBound :: CInt)
            minCInt = toInteger (minBound :: CInt)

sxShow :: SX -> String
{-# NOINLINE sxShow #-}
sxShow (SX s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxShow stringRef (fromIntegral stringLength)
  peekCString stringRef

sxEqual :: SX -> SX -> Bool
{-# NOINLINE sxEqual #-}
sxEqual (SX sx0) (SX sx1) = unsafePerformIO $ do
  equalInt <- withForeignPtrs2 c_sxEqual sx0 sx1
  let equalBool
        | equalInt == 0 = False
        | otherwise     = True
  return equalBool

sxPlus :: SX -> SX -> SX
{-# NOINLINE sxPlus #-}
sxPlus (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxPlus sx0 sx1 sxOut
  return (SX sxOut)

sxMinus :: SX -> SX -> SX
{-# NOINLINE sxMinus #-}
sxMinus (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxMinus sx0 sx1 sxOut
  return (SX sxOut)

sxTimes :: SX -> SX -> SX
{-# NOINLINE sxTimes #-}
sxTimes (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxTimes sx0 sx1 sxOut
  return (SX sxOut)

sxDivide :: SX -> SX -> SX
{-# NOINLINE sxDivide #-}
sxDivide (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxDivide sx0 sx1 sxOut
  return (SX sxOut)

sxNegate :: SX -> SX
{-# NOINLINE sxNegate #-}
sxNegate (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxNegate sx sxOut
  return (SX sxOut)

sxAbs :: SX -> SX
{-# NOINLINE sxAbs #-}
sxAbs (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxAbs sx sxOut
  return (SX sxOut)

sxSignum :: SX -> SX
{-# NOINLINE sxSignum #-}
sxSignum (SX sx) = unsafePerformIO $ do
  sign <- withForeignPtr sx c_sxSignum
  if (sign == 1)
    then sxNewInt 1
    else sxNewInt (-1)



sxPi :: SX
{-# NOINLINE sxPi #-}
sxPi = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtr sxOut $ c_sxPi
  return (SX sxOut)

sxExp :: SX -> SX
{-# NOINLINE sxExp #-}
sxExp (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxExp sx sxOut
  return (SX sxOut)

sxSqrt :: SX -> SX
{-# NOINLINE sxSqrt #-}
sxSqrt (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxSqrt sx sxOut
  return (SX sxOut)

sxLog :: SX -> SX
{-# NOINLINE sxLog #-}
sxLog (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxLog sx sxOut
  return (SX sxOut)

sxPow :: SX -> SX -> SX
{-# NOINLINE sxPow #-}
sxPow (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxPow sx0 sx1 sxOut
  return (SX sxOut)

sxSin :: SX -> SX
{-# NOINLINE sxSin #-}
sxSin (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxSin sx sxOut
  return (SX sxOut)

sxCos :: SX -> SX
{-# NOINLINE sxCos #-}
sxCos (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxCos sx sxOut
  return (SX sxOut)

sxTan :: SX -> SX
{-# NOINLINE sxTan #-}
sxTan (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxTan sx sxOut
  return (SX sxOut)

sxArcsin :: SX -> SX
{-# NOINLINE sxArcsin #-}
sxArcsin (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxArcsin sx sxOut
  return (SX sxOut)

sxArccos :: SX -> SX
{-# NOINLINE sxArccos #-}
sxArccos (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxArccos sx sxOut
  return (SX sxOut)

sxArctan :: SX -> SX
{-# NOINLINE sxArctan #-}
sxArctan (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxArctan sx sxOut
  return (SX sxOut)



sxFromInt :: Int -> SX
{-# NOINLINE sxFromInt #-}
sxFromInt n = unsafePerformIO $ do
  sxOut <- sxNewInt n
  return sxOut

sxFromIntegral :: Integral a => a -> SX
{-# NOINLINE sxFromIntegral #-}
sxFromIntegral n = unsafePerformIO $ do
  sxOut <- sxNewIntegral n
  return sxOut

sxFromDouble :: Double -> SX
{-# NOINLINE sxFromDouble #-}
sxFromDouble val = unsafePerformIO $ do
  s <- sxNewDouble val
  return s


sxBound :: SX -> (SX, SX) -> SX
{-# NOINLINE sxBound #-}
sxBound (SX sxIn) (SX sxLb, SX sxUb) = unsafePerformIO $ do
  (SX sxOut) <- sxNewDouble 0
  
  let sxLb'  = unsafeForeignPtrToPtr sxLb
      sxUb'  = unsafeForeignPtrToPtr sxUb
      sxIn'  = unsafeForeignPtrToPtr sxIn
      sxOut' = unsafeForeignPtrToPtr sxOut
  
  c_sxBound sxLb' sxUb' sxIn' sxOut'
  
  touchForeignPtr sxLb
  touchForeignPtr sxUb
  touchForeignPtr sxIn
  touchForeignPtr sxOut
  
  return (SX sxOut)


-- typeclass stuff
instance Eq SX where
  (==) = sxEqual
  (/=) sx0 sx1 = not $ sx0 == sx1

instance Show SX where
  show sx = sxShow sx

instance Num SX where
  (+) = sxPlus
  (*) = sxTimes
  (-) = sxMinus
  negate = sxNegate
  abs = sxAbs
  signum = sxSignum
  fromInteger = sxFromIntegral

instance Fractional SX where
  (/) = sxDivide
  recip sx = (sxFromInt 1)/sx
--  fromRational x = (sxFromIntegral num)/(sxFromIntegral den)
--    where
--      num = numerator x
--      den = denominator x
  fromRational x = sxFromDouble (fromRational x)

instance Floating SX where
  pi = sxPi
  exp = sxExp
  sqrt = sxSqrt
  log = sxLog
  (**) = sxPow
--    logBase :: a -> a -> a
  sin  = sxSin
  tan  = sxTan
  cos  = sxCos
  asin = sxArcsin
  atan = sxArctan
  acos = sxArccos
  sinh  = error "hyperbolic functions not yet implemented for SX"
  tanh  = error "hyperbolic functions not yet implemented for SX"
  cosh  = error "hyperbolic functions not yet implemented for SX"
  asinh = error "hyperbolic functions not yet implemented for SX"
  atanh = error "hyperbolic functions not yet implemented for SX"
  acosh = error "hyperbolic functions not yet implemented for SX"

instance Boundable SX where
  bound = sxBound
