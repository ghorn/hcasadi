-- SX.hs

--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

--module Main where
module Casadi.SX(SX(..), SXRaw(..), sxNewInteger) where

import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Data.Ratio(numerator, denominator)

-- the SX data type
data SXRaw = SXRaw
newtype SX = SX (ForeignPtr SXRaw)
--data SX = SX (ForeignPtr SXRaw) | SX Integer | SX Double

-- foreign imports
foreign import ccall "sxInterface.hpp sxNewDouble" c_sxNewDouble :: CDouble -> IO (Ptr SXRaw)
foreign import ccall "sxInterface.hpp sxNewInt"    c_sxNewInt :: CInt -> IO (Ptr SXRaw)
foreign import ccall "sxInterface.hpp &sxDelete"   c_sxDelete :: FunPtr (Ptr SXRaw -> IO ())
foreign import ccall "sxInterface.hpp sxShow"      c_sxShow :: Ptr CChar -> CInt -> (Ptr SXRaw) -> IO ()

foreign import ccall "sxInterface.hpp sxEqual"  c_sxEqual :: Ptr SXRaw -> Ptr SXRaw -> IO CInt
foreign import ccall "sxInterface.hpp sxPlus"   c_sxPlus :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxMinus"  c_sxMinus :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxTimes"  c_sxTimes :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxDivide" c_sxDivide :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxNegate" c_sxNegate :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxAbs"    c_sxAbs :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxSignum" c_sxSignum :: Ptr SXRaw -> IO CInt

foreign import ccall "sxInterface.hpp sxPi"     c_sxPi     :: Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxExp"    c_sxExp    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxSqrt"   c_sxSqrt   :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxLog"    c_sxLog    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxPow"    c_sxPow    :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxSin"    c_sxSin    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxCos"    c_sxCos    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxTan"    c_sxTan    :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxArcsin" c_sxArcsin :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxArccos" c_sxArccos :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall "sxInterface.hpp sxArctan" c_sxArctan :: Ptr SXRaw -> Ptr SXRaw -> IO ()


-- cpp function wrappers
sxNewDouble :: Double -> IO SX
sxNewDouble val = mask_ $ do
    f <- c_sxNewDouble (realToFrac val) >>= newForeignPtr c_sxDelete
    return $ SX f

sxNewInt :: Int -> IO SX
sxNewInt val = mask_ $ do
    f <- c_sxNewInt (fromIntegral val) >>= newForeignPtr c_sxDelete
    return $ SX f


sxNewInteger :: Integer -> IO SX
sxNewInteger val
      | withinCIntBounds val = sxNewInt (fromInteger val)
      | otherwise            = error "input out of range of CInt in sxNewInteger"
        where
            withinCIntBounds x = and [x <= maxCInt, x >= minCInt]
            maxCInt = fromIntegral (maxBound :: CInt)
            minCInt = fromIntegral (minBound :: CInt)

sxShow :: SX -> String
sxShow (SX s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxShow stringRef (fromIntegral stringLength)
  peekCString stringRef

sxEqual :: SX -> SX -> Bool
sxEqual (SX sx0) (SX sx1) = unsafePerformIO $ do
  equalInt <- withForeignPtrs2 c_sxEqual sx0 sx1
  let equalBool
        | equalInt == 0 = False
        | otherwise     = True
  return equalBool

sxPlus :: SX -> SX -> SX
sxPlus (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs3 c_sxPlus sx0 sx1 sxOut
  return (SX sxOut)

sxMinus :: SX -> SX -> SX
sxMinus (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs3 c_sxMinus sx0 sx1 sxOut
  return (SX sxOut)

sxTimes :: SX -> SX -> SX
sxTimes (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs3 c_sxTimes sx0 sx1 sxOut
  return (SX sxOut)

sxDivide :: SX -> SX -> SX
sxDivide (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs3 c_sxDivide sx0 sx1 sxOut
  return (SX sxOut)

sxNegate :: SX -> SX
sxNegate (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxNegate sx sxOut
  return (SX sxOut)

sxAbs :: SX -> SX
sxAbs (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxAbs sx sxOut
  return (SX sxOut)

sxSignum :: SX -> SX
sxSignum (SX sx) = unsafePerformIO $ do
  sign <- withForeignPtr sx c_sxSignum
  if (sign == 1)
    then sxNewInteger 1
    else sxNewInteger (-1)



sxPi :: SX
sxPi = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtr sxOut $ c_sxPi
  return (SX sxOut)

sxExp :: SX -> SX
sxExp (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxExp sx sxOut
  return (SX sxOut)

sxSqrt :: SX -> SX
sxSqrt (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxSqrt sx sxOut
  return (SX sxOut)

sxLog :: SX -> SX
sxLog (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxLog sx sxOut
  return (SX sxOut)

sxPow :: SX -> SX -> SX
sxPow (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs3 c_sxPow sx0 sx1 sxOut
  return (SX sxOut)

sxSin :: SX -> SX
sxSin (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxSin sx sxOut
  return (SX sxOut)

sxCos :: SX -> SX
sxCos (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxCos sx sxOut
  return (SX sxOut)

sxTan :: SX -> SX
sxTan (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxTan sx sxOut
  return (SX sxOut)

sxArcsin :: SX -> SX
sxArcsin (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxArcsin sx sxOut
  return (SX sxOut)

sxArccos :: SX -> SX
sxArccos (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxArccos sx sxOut
  return (SX sxOut)

sxArctan :: SX -> SX
sxArctan (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInteger 0
  withForeignPtrs2 c_sxArctan sx sxOut
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
  fromInteger val = unsafePerformIO $ sxNewInteger val

instance Fractional SX where
  (/) = sxDivide
  recip sx = (unsafePerformIO $ sxNewInteger 1)/sx
  fromRational x = (unsafePerformIO $ sxNewInteger num)/(unsafePerformIO $ sxNewInteger den)
    where
      num = numerator x
      den = denominator x

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

main :: IO ()
main = do 
  f <- sxNewDouble 10.1
  g <- sxNewDouble 3.0
  h <- sxNewDouble (-4.4)

  putStrLn $ sxShow f
  putStrLn $ sxShow g
  putStrLn $ sxShow (sxPlus f g)
  putStrLn $ sxShow (sxTimes f g)
  putStrLn $ sxShow (sxMinus f g)
  putStrLn $ sxShow (sxNegate f)
  putStrLn $ sxShow (sxAbs f)
  putStrLn $ sxShow (sxAbs h)
  putStrLn $ sxShow (sxSignum f)
  putStrLn $ sxShow (sxSignum h)

  putStrLn $ sxShow (sxDivide f g)

  putStrLn $ sxShow (sin f)
  putStrLn $ sxShow (cos g)
  putStrLn $ sxShow (tan h)
  putStrLn $ sxShow (asin f)
  putStrLn $ sxShow (acos g)
  putStrLn $ sxShow (atan h)
