-- SXMatrix.hs

--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

--module SXMatrix(SXMatrix(..), SXMatrixRaw) where
module Main where

import SX
import CasadiInterfaceUtils

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)

-- the SXMatrix data type
data SXMatrixRaw = SXMatrixRaw
newtype SXMatrix = SXMatrix (ForeignPtr SXMatrixRaw)

-- foreign imports
foreign import ccall "sxMatrixCreateSymbolic" c_sxMatrixCreateSymbolic :: Ptr CChar -> CInt -> CInt -> IO (Ptr SXMatrixRaw)
foreign import ccall "sxVectorCreateSymbolic" c_sxVectorCreateSymbolic :: Ptr CChar -> CInt -> IO (Ptr SXMatrixRaw)
foreign import ccall "&sxMatrixDelete" c_sxMatrixDelete :: FunPtr (Ptr SXMatrixRaw -> IO ())
foreign import ccall "sxMatrixShow" c_sxMatrixShow :: Ptr CChar -> CInt -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMatrixAt" c_sxMatrixAt :: (Ptr SXMatrixRaw) -> CInt -> CInt -> (Ptr SXRaw) -> IO ()
foreign import ccall "sxVectorAt" c_sxVectorAt :: (Ptr SXMatrixRaw) -> CInt -> (Ptr SXRaw) -> IO ()

-- cpp function wrappers
sxVectorCreateSymbolic :: String -> Integer -> IO SXMatrix
sxVectorCreateSymbolic string n = mask_ $ do
  cString <- newCString string
  f <- c_sxVectorCreateSymbolic cString (fromIntegral n) >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix f

sxMatrixCreateSymbolic :: String -> (Integer, Integer) -> IO SXMatrix
sxMatrixCreateSymbolic string (n,m) = mask_ $ do
  cString <- newCString string
  f <- c_sxMatrixCreateSymbolic cString (fromIntegral n) (fromIntegral m) >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix f

sxMatrixShow :: SXMatrix -> String
sxMatrixShow (SXMatrix s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxMatrixShow stringRef (fromIntegral stringLength)
  peekCString stringRef

sxMatrixAt :: SXMatrix -> (Integer,Integer) -> SX
sxMatrixAt (SXMatrix matIn) (n,m) = unsafePerformIO $ do
  (SX sxOut) <- sxNewInteger 0
  withForeignPtrs2 (\matIn' sxOut' -> c_sxMatrixAt matIn' (fromIntegral n) (fromIntegral m) sxOut') matIn sxOut
  return (SX sxOut)



instance Show SXMatrix where
  show sx = sxMatrixShow sx

main :: IO ()
main = do 
  f <- sxMatrixCreateSymbolic "test_yo" (10,3)
  v <- sxVectorCreateSymbolic "vest_yo" 4
  print f
  print v
