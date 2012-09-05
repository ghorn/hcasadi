-- SXMatrix.hs

{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-cse -fno-full-laziness #-}
{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}

module Casadi.SXMatrix
       (
         SXMatrix(..)
       , SXMatrixRaw(..)
       , sxMatrixCreateSymbolic
       , sxMatrixNewZeros
       , gradient
       , hessian
       , jacobian
       ) where

--import Casadi.SX
import Casadi.SXFunctionRaw
import Casadi.Bindings.CasadiInterfaceUtils
import Casadi.Matrix

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq

-- the SXMatrix data type
data SXMatrixRaw = SXMatrixRaw
newtype SXMatrix = SXMatrix (ForeignPtr SXMatrixRaw)

instance NFData SXMatrix where
  rnf x = x `seq` ()

-- foreign imports
foreign import ccall unsafe "sxMatrixDuplicate" c_sxMatrixDuplicate
  :: (Ptr SXMatrixRaw) -> IO (Ptr SXMatrixRaw)
foreign import ccall unsafe "sxMatrixZeros" c_sxMatrixZeros
  :: CInt -> CInt -> IO (Ptr SXMatrixRaw)
foreign import ccall unsafe "sxMatrixShow" c_sxMatrixShow
  :: Ptr CChar -> CInt -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall unsafe "sxMatrixAt" c_sxMatrixAt
  :: (Ptr SXMatrixRaw) -> CInt -> CInt -> (Ptr SXRaw) -> IO ()
foreign import ccall unsafe "sxMatrixSet" c_sxMatrixSet
  :: (Ptr SXRaw) -> CInt -> CInt -> (Ptr SXMatrixRaw) -> IO ()

foreign import ccall unsafe "myGradient" c_myGradient
  :: (Ptr SXRaw) -> (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall unsafe "myHessian" c_myHessian
  :: (Ptr SXRaw) -> (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall unsafe "myJacobian" c_myJacobian
  :: (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()

foreign import ccall unsafe "sxFunctionEvaluateSXMatrix" c_sxFunctionEvaluateSXMatrix
  :: CInt -> Ptr (Ptr SXMatrixRaw) -> CInt -> Ptr (Ptr SXMatrixRaw) -> Ptr SXFunctionRaw -> IO ()

sxMatrixDuplicate :: SXMatrix -> IO SXMatrix
sxMatrixDuplicate (SXMatrix old) = mask_ $ do
  new <- withForeignPtr old c_sxMatrixDuplicate >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix new


sxMatrixNewZeros :: (Int, Int) -> IO SXMatrix
sxMatrixNewZeros (n,m) = mask_ $ do
  let n' = safeToCInt n
      m' = safeToCInt m
      
      safeToCInt :: Int -> CInt
      safeToCInt x
        | and [toInteger x <= maxCInt, toInteger x >= minCInt] = fromIntegral x
        | otherwise = error "Error - sxMatrixNewZeros dimensions too big"
        where
          maxCInt = fromIntegral (maxBound :: CInt)
          minCInt = fromIntegral (minBound :: CInt)

  mat <- c_sxMatrixZeros n' m' >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix mat

sxMatrixZeros :: (Int,Int) -> SXMatrix
{-# NOINLINE sxMatrixZeros #-}
sxMatrixZeros dim = unsafePerformIO $ do
  mat <- sxMatrixNewZeros dim
  return mat


sxMatrixFromList :: [SX] -> SXMatrix
{-# NOINLINE sxMatrixFromList #-}
sxMatrixFromList sxList = unsafePerformIO $ do
  m0 <- sxMatrixNewZeros (length sxList, 1)
  let indexedSXList = zip sxList $ take (length sxList) [0..]
  return $ foldl (\acc (sx,idx) -> sxMatrixSet acc (idx,0) sx) m0 indexedSXList

sxMatrixToLists :: SXMatrix -> [[SX]]
{-# NOINLINE sxMatrixToLists #-}
sxMatrixToLists mat = unsafePerformIO $ do
  let f row = mapM (\col -> sxMatrixAt mat (row, col)) [0..m-1]
      (n,m) = sxMatrixSize mat
  mapM f [0..n-1]


-- turns n by 1 matrix into a list of SX, returns error if matrix is not n by 1
sxMatrixToList :: SXMatrix -> [SX]
{-# NOINLINE sxMatrixToList #-}
sxMatrixToList mat = unsafePerformIO $ do
  let (n,m) = sxMatrixSize mat
  if m == 1
    then mapM (\row -> sxMatrixAt mat (row, 0)) [0..n-1]
    else error "sxMatrixToList can only be used on an n by 1 matrix"

----------------- ad -----------------------
gradient :: SXMatrix -> SXMatrix -> SXMatrix
{-# NOINLINE gradient #-}
gradient expr (SXMatrix argsRaw) = unsafePerformIO $ do
  if (1,1) /= size expr
    then do error $ "error: can't take gradient of non-scalar, dimensions: " ++ show (size expr)
    else do let [(SX expRaw)] = toList expr
            SXMatrix mOut <- sxMatrixNewZeros (1,1)
            withForeignPtrs3 c_myGradient expRaw argsRaw mOut
            return $ (SXMatrix mOut)

hessian :: SXMatrix -> SXMatrix -> SXMatrix
{-# NOINLINE hessian #-}
hessian expr (SXMatrix argsRaw) = unsafePerformIO $ do
  if (1,1) /= size expr
    then do error $ "error: can't take hessian of non-scalar, dimensions: " ++ show (size expr)
    else do let [(SX expRaw)] = toList expr
            SXMatrix mOut <- sxMatrixNewZeros (1,1)
            withForeignPtrs3 c_myHessian expRaw argsRaw mOut
            return $ (SXMatrix mOut)

jacobian :: SXMatrix -> SXMatrix -> SXMatrix
{-# NOINLINE jacobian #-}
jacobian (SXMatrix expRaw) (SXMatrix argsRaw) = unsafePerformIO $ do
  SXMatrix mOut <- sxMatrixNewZeros (1,1)
  withForeignPtrs3 c_myJacobian expRaw argsRaw mOut
  return $ (SXMatrix mOut)
