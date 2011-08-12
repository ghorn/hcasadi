-- SXFunction.hs

--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SXFunction(SXFunction(..), SXFunctionRaw) where
--module Main where

import SXMatrix
import CasadiInterfaceUtils

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Data.Ratio(numerator, denominator)

-- the SXFunction data type
data SXFunctionRaw = SXFunctionRaw
newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)

-- foreign imports
foreign import ccall "sxFunctionCreate" c_sxFunctionCreate :: Ptr SXMatrixRaw -> Ptr SXMatrixRaw -> IO (Ptr SXFunctionRaw)
foreign import ccall "&sxFunctionDelete" c_sxFunctionDelete :: FunPtr (Ptr SXFunctionRaw -> IO ())


-- cpp function wrappers
sxFunctionCreate :: SXMatrix -> SXMatrix -> IO SXFunction
sxFunctionCreate m0 m1 = mask_ $ do
  f <- withForeignPtrs2 c_sxFunctionCreate m0 m1 >>= newForeignPtr c_sxFunctionDelete
  return $ SXFunction f


main :: IO ()
main = do
  f <- sxFunctionCreateSymbolic "test_yo" 10
  print f
