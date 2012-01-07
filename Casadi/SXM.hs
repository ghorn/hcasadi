-- SXM.hs

{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-cse -fno-full-laziness #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.SXM( SXM(..)
                 , SXMRaw(..)
--                 , sxNewDouble
--                 , sxNewInt
--                 , sxNewIntegral
--                 , sxFromInt
--                 , sxFromIntegral
--                 , sxFromDouble
                 , sym
                 , symVec
                 , symMat
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
data SXMRaw = SXMRaw
newtype SXM = SXM (ForeignPtr SXMRaw)

instance NFData SXM where
  rnf x = x `seq` ()

-- foreign imports
foreign import ccall unsafe "sxMatMisc.hpp createSymbolic" c_sxCreateSymbolic :: CInt -> CInt -> Ptr CChar -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxMatMisc.hpp newDouble" c_sxNewDouble :: CDouble -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxMatMisc.hpp newInt"    c_sxNewInt :: CInt -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxMatMisc.hpp &sxDelete" c_sxDelete :: FunPtr (Ptr SXMRaw -> IO ())
foreign import ccall unsafe "sxMatMisc.hpp sxShow"    c_sxShow :: Ptr CChar -> CInt -> (Ptr SXMRaw) -> IO ()

foreign import ccall unsafe "sxMatMath.hpp sxEqual"  c_sxEqual :: Ptr SXMRaw -> Ptr SXMRaw -> IO CInt
foreign import ccall unsafe "sxMatMath.hpp sxSignum" c_sxSignum :: Ptr SXMRaw -> IO CInt
foreign import ccall unsafe "sxMatMisc.hpp sxBound"  c_sxBound :: Ptr SXMRaw -> Ptr SXMRaw -> Ptr SXMRaw -> Ptr SXMRaw -> IO ()

-- binary
foreign import ccall unsafe "sxmBinary.hpp sxPlus"   c_sxPlus :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmBinary.hpp sxMinus"  c_sxMinus :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmBinary.hpp sxTimes"  c_sxTimes :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmBinary.hpp sxDivide" c_sxDivide :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmBinary.hpp sxPow"    c_sxPow    :: Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)

-- unary
foreign import ccall unsafe "sxmUnary.hpp sxNegate" c_sxNegate :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxAbs"    c_sxAbs    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxExp"    c_sxExp    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxSqrt"   c_sxSqrt   :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxLog"    c_sxLog    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxSin"    c_sxSin    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxCos"    c_sxCos    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxTan"    c_sxTan    :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxArcsin" c_sxArcsin :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxArccos" c_sxArccos :: Ptr SXMRaw -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxmUnary.hpp sxArctan" c_sxArctan :: Ptr SXMRaw -> IO (Ptr SXMRaw)


------------------- binary -------------------
sxWrapBinary :: (Ptr SXMRaw -> Ptr SXMRaw -> IO (Ptr SXMRaw)) -> SXM -> SXM -> SXM
{-# NOINLINE sxWrapBinary #-}
sxWrapBinary c_fun (SXM sxm0Raw) (SXM sxm1Raw) = unsafePerformIO $ mask_ $ do
  smxOutRaw <- withForeignPtrs2 c_fun sxm0Raw sxm1Raw >>= newForeignPtr c_sxDelete
  return $ SXM smxOutRaw

sxPlus :: SXM -> SXM -> SXM
sxPlus = sxWrapBinary c_sxPlus

sxMinus :: SXM -> SXM -> SXM
sxMinus = sxWrapBinary c_sxMinus

sxTimes :: SXM -> SXM -> SXM
sxTimes = sxWrapBinary c_sxTimes

sxDivide :: SXM -> SXM -> SXM
sxDivide = sxWrapBinary c_sxDivide

sxPow :: SXM -> SXM -> SXM
sxPow = sxWrapBinary c_sxPow


---------------------- unary ----------------------
sxWrapUnary :: (Ptr SXMRaw -> IO (Ptr SXMRaw)) -> SXM -> SXM
{-# NOINLINE sxWrapUnary #-}
sxWrapUnary c_fun (SXM sxmInRaw) = unsafePerformIO $ mask_ $ do
  smxOutRaw <- withForeignPtr sxmInRaw c_fun >>= newForeignPtr c_sxDelete
  return $ SXM smxOutRaw

sxNegate :: SXM -> SXM
sxNegate = sxWrapUnary c_sxNegate

sxAbs :: SXM -> SXM
sxAbs = sxWrapUnary c_sxAbs

sxExp :: SXM -> SXM
sxExp = sxWrapUnary c_sxExp

sxSqrt :: SXM -> SXM
sxSqrt = sxWrapUnary c_sxSqrt

sxLog :: SXM -> SXM
sxLog = sxWrapUnary c_sxLog

sxSin :: SXM -> SXM
sxSin = sxWrapUnary c_sxSin

sxCos :: SXM -> SXM
sxCos = sxWrapUnary c_sxCos

sxTan :: SXM -> SXM
sxTan = sxWrapUnary c_sxTan

sxArcsin :: SXM -> SXM
sxArcsin = sxWrapUnary c_sxArcsin

sxArccos :: SXM -> SXM
sxArccos = sxWrapUnary c_sxArccos

sxArctan :: SXM -> SXM
sxArctan = sxWrapUnary c_sxArctan


------------------- create symbolic -------------------------------
sym :: String -> IO SXM
sym name = mask_ $ do
  cName <- newCString name
  sym' <- c_sxCreateSymbolic 1 1 cName >>= newForeignPtr c_sxDelete
  return $ SXM sym'

symVec :: Int -> String -> IO SXM
symVec n name = mask_ $ do
  cName <- newCString name
  sym' <- c_sxCreateSymbolic (fromIntegral n) 1 cName >>= newForeignPtr c_sxDelete
  return $ SXM sym'

symMat :: (Int,Int) -> String -> IO SXM
symMat (n,m) name = mask_ $ do
  cName <- newCString name
  sym' <- c_sxCreateSymbolic (fromIntegral n) (fromIntegral m) cName >>= newForeignPtr c_sxDelete
  return $ SXM sym'


-------------------- create numeric -----------------------
sxNewDouble :: Double -> IO SXM
sxNewDouble val = mask_ $ do
    f <- c_sxNewDouble (realToFrac val) >>= newForeignPtr c_sxDelete
    return $ SXM f

sxNewInt :: Int -> IO SXM
sxNewInt val = mask_ $ do
    f <- c_sxNewInt (fromIntegral val) >>= newForeignPtr c_sxDelete
    return $ SXM f

sxNewIntegral :: Integral a => a -> IO SXM
sxNewIntegral val
      | withinCIntBounds val = sxNewInt (fromIntegral val)
      | otherwise            = error "input out of range of CInt in sxNewIntegral"
        where
            withinCIntBounds x = and [fromIntegral x <= maxCInt, fromIntegral x >= minCInt]
            maxCInt = toInteger (maxBound :: CInt)
            minCInt = toInteger (minBound :: CInt)

-------------------- misc -----------------
sxEqual :: SXM -> SXM -> Bool
{-# NOINLINE sxEqual #-}
sxEqual (SXM sx0) (SXM sx1) = unsafePerformIO $ do
  equalInt <- withForeignPtrs2 c_sxEqual sx0 sx1
  let equalBool
        | equalInt == 0 = False
        | otherwise     = True
  return equalBool

sxSignum :: SXM -> SXM
{-# NOINLINE sxSignum #-}
sxSignum (SXM sx) = unsafePerformIO $ do
  sign <- withForeignPtr sx c_sxSignum
  if (sign == 1)
    then sxNewInt 1
    else sxNewInt (-1)

sxBound :: SXM -> (SXM, SXM) -> SXM
{-# NOINLINE sxBound #-}
sxBound (SXM sxIn) (SXM sxLb, SXM sxUb) = unsafePerformIO $ do
  (SXM sxOut) <- sxNewDouble 0
  
  let sxLb'  = unsafeForeignPtrToPtr sxLb
      sxUb'  = unsafeForeignPtrToPtr sxUb
      sxIn'  = unsafeForeignPtrToPtr sxIn
      sxOut' = unsafeForeignPtrToPtr sxOut
  
  c_sxBound sxLb' sxUb' sxIn' sxOut'
  
  touchForeignPtr sxLb
  touchForeignPtr sxUb
  touchForeignPtr sxIn
  touchForeignPtr sxOut
  
  return (SXM sxOut)


-- typeclass stuff
instance Eq SXM where
  (==) = sxEqual
  (/=) sx0 sx1 = not $ sx0 == sx1


instance Show SXM where
  {-# NOINLINE show #-}
  show (SXM s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxShow stringRef (fromIntegral stringLength)
  peekCString stringRef


instance Num SXM where
  (+) = sxPlus
  (*) = sxTimes
  (-) = sxMinus
  negate = sxNegate
  abs = sxAbs
  signum = sxSignum
  fromInteger = unsafePerformIO . sxNewIntegral

instance Fractional SXM where
  (/) = sxDivide
  recip sx = (unsafePerformIO $ sxNewIntegral (1 :: Int))/sx
  fromRational x = unsafePerformIO $ sxNewDouble $ fromRational x

instance Floating SXM where
  pi = unsafePerformIO $ sxNewDouble pi
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
  sinh  = error "hyperbolic functions not yet implemented for SXM"
  tanh  = error "hyperbolic functions not yet implemented for SXM"
  cosh  = error "hyperbolic functions not yet implemented for SXM"
  asinh = error "hyperbolic functions not yet implemented for SXM"
  atanh = error "hyperbolic functions not yet implemented for SXM"
  acosh = error "hyperbolic functions not yet implemented for SXM"

instance Boundable SXM where
  bound = sxBound
