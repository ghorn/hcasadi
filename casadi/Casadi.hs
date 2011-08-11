-- Casadi.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)

-- the SX data type
data SXRaw = SXRaw
newtype SX = SX (ForeignPtr SXRaw)


-- foreign imports
foreign import ccall unsafe "casadiInterface.hpp sxNewDouble" c_sxNewDouble :: CDouble -> IO (Ptr SXRaw)
foreign import ccall unsafe "casadiInterface.hpp sxNewInt" c_sxNewInt :: CInt -> IO (Ptr SXRaw)
foreign import ccall unsafe "casadiInterface.hpp &sxDelete" c_sxDelete :: FunPtr (Ptr SXRaw -> IO ())
foreign import ccall unsafe "casadiInterface.hpp sxShow" c_sxShow :: Ptr CChar -> CInt -> (Ptr SXRaw) -> IO ()
foreign import ccall unsafe "casadiInterface.hpp sxEqual" c_sxEqual :: Ptr SXRaw -> Ptr SXRaw -> IO CInt
foreign import ccall unsafe "casadiInterface.hpp sxPlus" c_sxPlus :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "casadiInterface.hpp sxTimes" c_sxTimes :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "casadiInterface.hpp sxMinus" c_sxMinus :: Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "casadiInterface.hpp sxNegate" c_sxNegate :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "casadiInterface.hpp sxAbs" c_sxAbs :: Ptr SXRaw -> Ptr SXRaw -> IO ()
foreign import ccall unsafe "casadiInterface.hpp sxSignum" c_sxSignum :: Ptr SXRaw -> IO CInt


-- convenience functions for cpp wrappers
withForeignPtrs2 :: (Ptr SXRaw -> Ptr SXRaw -> IO a) -> ForeignPtr SXRaw -> ForeignPtr SXRaw -> IO a
withForeignPtrs2 f0 p0 p1 = withForeignPtr p1 $ \p1' -> (f1 p1')
  where
    f1 p1' = withForeignPtr p0 (\p0' -> f0 p0' p1')

withForeignPtrs3 :: (Ptr SXRaw -> Ptr SXRaw -> Ptr SXRaw -> IO a) -> ForeignPtr SXRaw -> ForeignPtr SXRaw -> ForeignPtr SXRaw -> IO a
withForeignPtrs3 f0 p0 p1 p2 = withForeignPtr p2 $ \p2' -> (f2 p2')
  where
    f2 p2'     = withForeignPtr p1 (\p1' -> f1 p1' p2'    )
    f1 p1' p2' = withForeignPtr p0 (\p0' -> f0 p0' p1' p2')


-- cpp function wrappers
sxNewDouble :: Double -> IO SX
sxNewDouble val = mask_ $ do
    f <- c_sxNewDouble (realToFrac val) >>= newForeignPtr c_sxDelete
    return $ SX f

sxNewInt :: Int -> IO SX
sxNewInt val = mask_ $ do
    f <- c_sxNewInt (fromIntegral val) >>= newForeignPtr c_sxDelete
    return $ SX f

sxShow :: SX -> String
sxShow (SX s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxShow stringRef (fromIntegral stringLength)
  poop <- peekCString stringRef
  return poop

sxEqual :: SX -> SX -> Bool
sxEqual (SX sx0) (SX sx1) = unsafePerformIO $ do
  equalInt <- withForeignPtrs2 c_sxEqual sx0 sx1
  let equalBool
        | equalInt == 0 = False
        | otherwise     = True
  return equalBool

sxPlus :: SX -> SX -> SX
sxPlus (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxPlus sx0 sx1 sxOut
  return (SX sxOut)

sxTimes :: SX -> SX -> SX
sxTimes (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxTimes sx0 sx1 sxOut
  return (SX sxOut)

sxMinus :: SX -> SX -> SX
sxMinus (SX sx0) (SX sx1) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs3 c_sxMinus sx0 sx1 sxOut
  return (SX sxOut)

sxNegate :: SX -> SX
sxNegate (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxNegate sx sxOut
  return (SX sxOut)

sxAbs :: SX -> SX
sxAbs (SX sx) = unsafePerformIO $ do
  SX sxOut <- sxNewInt 0
  withForeignPtrs2 c_sxAbs sx sxOut
  return (SX sxOut)

sxSignum :: SX -> SX
sxSignum (SX sx) = unsafePerformIO $ do
  sign <- withForeignPtr sx c_sxSignum
  if (sign == 1)
    then sxNewInt 1
    else sxNewInt (-1)



-- typeclass stuff
instance Eq SX where
  (==) sx0 sx1 = sxEqual sx0 sx1
  (/=) sx0 sx1 = not $ sx0 == sx1

instance Show SX where
  show sx = sxShow sx


--instance Num SX where
--    (+) :: a -> a -> a
--    (*) :: a -> a -> a
--    (-) :: a -> a -> a
--    negate :: a -> a
--    abs :: a -> a
--    signum :: a -> a
--    fromInteger :: Integer -> a



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
