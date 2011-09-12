-- SXFunction.hs

{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-cse -fno-full-laziness #-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Casadi.SXFunction
       (
         SXFunction(..)
       , sxFunctionCreate
       , sxFunctionEvaluate
       , sxFunctionEvaluateLists
       , sxFunctionGetInputsSX
       , sxFunctionGetOutputsSX
       , sxFunctionGetInputDim
       , sxFunctionGetOutputDim
       , sxFunctionGradientAt
       , sxFunctionGradients
       , sxFunctionJacobianAt
       , sxFunctionHessianAt
       ) where

import Casadi.SXFunctionRaw(SXFunctionRaw(..))
import Casadi.SXMatrix
import Casadi.DMatrix
import Casadi.Matrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal(newArray)
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Text.Printf
import Control.DeepSeq

-- the SXFunction data type
newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)

instance NFData SXFunction where
  rnf x = x `seq` ()

-- foreign imports
foreign import ccall unsafe "sxFunctionCreate" c_sxFunctionCreate
  :: Ptr (Ptr SXMatrixRaw) -> CInt -> Ptr (Ptr SXMatrixRaw) -> CInt -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "&sxFunctionDelete" c_sxFunctionDelete
  :: FunPtr (Ptr SXFunctionRaw -> IO ())
foreign import ccall unsafe "sxFunctionGetNumInputs" c_sxFunctionGetNumInputs
  :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetNumOutputs" c_sxFunctionGetNumOutputs
  :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetInputsSX" c_sxFunctionGetInputsSX
  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGetOutputsSX" c_sxFunctionGetOutputsSX
  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGetInputSize1" c_sxFunctionGetInputSize1
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetInputSize2" c_sxFunctionGetInputSize2
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetOutputSize1" c_sxFunctionGetOutputSize1
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetOutputSize2" c_sxFunctionGetOutputSize2
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGradient" c_sxFunctionGradient
  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionJacobian" c_sxFunctionJacobian
  :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionHessian" c_sxFunctionHessian
  :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMatrixRaw -> IO ()


sxFunctionCreate :: [SXMatrix] -> [SXMatrix] -> SXFunction
{-# NOINLINE sxFunctionCreate #-}
sxFunctionCreate inputs outputs = unsafePerformIO $ mask_ $ do
  -- turn input/output SXMatrix lists into [Ptr SXMatrixRaw]
  let unsafeInputPtrs :: [Ptr SXMatrixRaw]
      unsafeInputPtrs = map (\(SXMatrix mat) -> unsafeForeignPtrToPtr mat) inputs
      
      unsafeOutputPtrs :: [Ptr SXMatrixRaw]
      unsafeOutputPtrs = map (\(SXMatrix mat) -> unsafeForeignPtrToPtr mat) outputs
      
      nIn  = fromIntegral $ length inputs
      nOut = fromIntegral $ length outputs
  
  -- turn [Ptr SXMatrixRaw] into Ptr (Ptr SXMatrixRaw)
  inputPtrArray <- newArray unsafeInputPtrs
  outputPtrArray <- newArray unsafeOutputPtrs
  
  -- create SXFunction
  funRaw <- c_sxFunctionCreate inputPtrArray nIn outputPtrArray nOut  >>= newForeignPtr c_sxFunctionDelete
  
  -- touch all [ForeignPtr SXMatrixRaw] for unsafeForeignPtrToPtr safety
  mapM_ (\(SXMatrix d) -> touchForeignPtr d) inputs
  mapM_ (\(SXMatrix d) -> touchForeignPtr d) outputs
  
  return (SXFunction funRaw)


------------------- getters -----------------------
sxFunctionNumInputs :: SXFunction -> Int
{-# NOINLINE sxFunctionNumInputs #-}
sxFunctionNumInputs (SXFunction fun) = unsafePerformIO $ do
  num <- withForeignPtr fun c_sxFunctionGetNumInputs
  return $ fromIntegral num


sxFunctionNumOutputs :: SXFunction -> Int
{-# NOINLINE sxFunctionNumOutputs #-}
sxFunctionNumOutputs (SXFunction fun) = unsafePerformIO $ do
  num <- withForeignPtr fun c_sxFunctionGetNumOutputs
  return $ fromIntegral num


sxFunctionGetInputsSX :: SXFunction -> Int -> SXMatrix
{-# NOINLINE sxFunctionGetInputsSX #-}
sxFunctionGetInputsSX (SXFunction fun) idx = unsafePerformIO $ do
  if idx >= sxFunctionNumInputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGetInputsSX - requested input index: %d >= numInputs (SXFunction fun): %d" idx (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetInputsSX fun' (fromIntegral idx) mat') fun mat
  return (SXMatrix mat)


sxFunctionGetOutputsSX :: SXFunction -> Int -> SXMatrix
{-# NOINLINE sxFunctionGetOutputsSX #-}
sxFunctionGetOutputsSX (SXFunction fun) idx = unsafePerformIO $ do
  if idx >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGetOutputsSX - requested output index: %d >= numOutputs (SXFunction fun): %d" idx (sxFunctionNumOutputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetOutputsSX fun' (fromIntegral idx) mat') fun mat
  return (SXMatrix mat)


sxFunctionGetInputDim :: SXFunction -> Int -> (Int, Int)
{-# NOINLINE sxFunctionGetInputDim #-}
sxFunctionGetInputDim (SXFunction fun) idx = unsafePerformIO $ do
  size1 <- withForeignPtr fun $ c_sxFunctionGetInputSize1 (fromIntegral idx)
  size2 <- withForeignPtr fun $ c_sxFunctionGetInputSize2 (fromIntegral idx)
  return (fromIntegral size1, fromIntegral size2)


sxFunctionGetOutputDim :: SXFunction -> Int -> (Int, Int)
{-# NOINLINE sxFunctionGetOutputDim #-}
sxFunctionGetOutputDim (SXFunction fun) idx = unsafePerformIO $ do
  size1 <- withForeignPtr fun $ c_sxFunctionGetOutputSize1 (fromIntegral idx)
  size2 <- withForeignPtr fun $ c_sxFunctionGetOutputSize2 (fromIntegral idx)
  return (fromIntegral size1, fromIntegral size2)


----------------------- AD -----------------------
sxFunctionGradientAt :: SXFunction -> Int -> SXMatrix
{-# NOINLINE sxFunctionGradientAt #-}
sxFunctionGradientAt (SXFunction fun) idxInput = unsafePerformIO $ do
  -- don't take gradient with respect to non-existant input
  if idxInput >= sxFunctionNumInputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGradientAt - requested gradient index: %d >= numInputs (SXFunction fun): %d" idxInput (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  -- don't take gradient of vector valued function
  if (1,1) /= (size $ sxFunctionGetOutputsSX (SXFunction fun) 0)
    then error $ printf "Error in sxFunctionGradientAt - requested gradient of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGradient fun' (fromIntegral idxInput) mat') fun mat
  return $ (SXMatrix mat)


sxFunctionGradients :: SXFunction -> [SXMatrix]
sxFunctionGradients fun = map (sxFunctionGradientAt fun) $ take (sxFunctionNumInputs fun) [0..]


sxFunctionJacobianAt :: SXFunction -> (Int, Int) -> SXMatrix
{-# NOINLINE sxFunctionJacobianAt #-}
sxFunctionJacobianAt (SXFunction fun) (idx0, idx1) = unsafePerformIO $ do
  -- don't take jacobian with respect to non-existant output
  if idx0 >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numOutputs (SXFunction fun): %d" idx0 idx1 (sxFunctionNumOutputs (SXFunction fun))
    else do return ()

  -- don't take jacobian with respect to non-existant input
  if idx1 >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numInputs (SXFunction fun): %d" idx0 idx1 (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionJacobian fun' (fromIntegral idx0) (fromIntegral idx1) mat') fun mat
  return $ (SXMatrix mat)


sxFunctionHessianAt :: SXFunction -> (Int, Int) -> SXMatrix
{-# NOINLINE sxFunctionHessianAt #-}
sxFunctionHessianAt (SXFunction fun) (idx0, idx1) = unsafePerformIO $ do
  -- don't take hessian with respect to non-existant input
  if any (\x -> x >= sxFunctionNumInputs (SXFunction fun)) [idx0, idx1]
    then error $ printf "Error in sxFunctionHessianAt - requested hessian index: (%d,%d) >= numInputs (SXFunction fun): %d" idx0 idx1 (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  -- don't take hessian of vector valued function
  if (1,1) /= (size $ sxFunctionGetOutputsSX (SXFunction fun) 0)
    then error $ printf "Error in sxFunctionHessianAt - requested hessian of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionHessian fun' (fromIntegral idx0) (fromIntegral idx1) mat') fun mat
  return $ (SXMatrix mat)


--------------------- evaluate -----------------------------
sxFunctionEvaluate :: forall a b c. Matrix a b c => SXFunction -> [a] -> [a]
{-# NOINLINE sxFunctionEvaluate #-}
sxFunctionEvaluate fun@(SXFunction funRaw) inputs = unsafePerformIO $ do
  let unsafeInputPtrs :: [Ptr c]
      unsafeInputPtrs = map (\m -> unsafeForeignPtrToPtr (getForeignPtr m)) inputs
      numOutputs = sxFunctionNumOutputs fun
  outputs <- mapM (\idx -> newZeros (sxFunctionGetOutputDim fun idx)) [0..numOutputs - 1]
  let unsafeOutputPtrs :: [Ptr c]
      unsafeOutputPtrs = map (\m -> unsafeForeignPtrToPtr (getForeignPtr m)) outputs
  
  inputPtrArray <- newArray unsafeInputPtrs
  outputPtrArray <- newArray unsafeOutputPtrs
  
  let nIn = fromIntegral $ length inputs
      nOut = fromIntegral$ length outputs

  let eval :: CInt -> Ptr (Ptr c) -> CInt -> Ptr (Ptr c) -> Ptr SXFunctionRaw -> IO ()
      eval = c_sxFunctionEvaluate (head inputs)
  withForeignPtr funRaw (eval nIn inputPtrArray nOut outputPtrArray)
  
  mapM_ (\d -> touchForeignPtr (getForeignPtr d)) inputs
  mapM_ (\d -> touchForeignPtr (getForeignPtr d)) outputs
  
  return outputs


sxFunctionEvaluateLists :: SXFunction -> [[[Double]]] -> [[[Double]]]
{-# NOINLINE sxFunctionEvaluateLists #-}
sxFunctionEvaluateLists fun inputs = unsafePerformIO $ do
  let outNew = map toLists $ sxFunctionEvaluate fun $ (map fromLists inputs :: [DMatrix])
  return outNew
