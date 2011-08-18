-- SXFunction.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.SXFunction
       (
         SXFunction(..)
       , SXFunctionRaw(..)
       , sxFunctionCreate
       , sxFunctionCreate'
       , sxFunctionEvaluate
       , sxFunctionGetInputs
       , sxFunctionGetOutputs
       , sxFunctionGradientAt
       , sxFunctionGradients
       , sxFunctionJacobianAt
       , sxFunctionHessianAt
       ) where

import Casadi.SXMatrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal(newArray, mallocArray, peekArray)
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)

import Text.Printf

-- the SXFunction data type
data SXFunctionRaw = SXFunctionRaw
newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)

-- foreign imports
foreign import ccall unsafe "sxFunctionCreate" c_sxFunctionCreate :: Ptr SXMatrixRaw -> Ptr SXMatrixRaw -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "sxFunctionCreateMulti" c_sxFunctionCreateMulti :: (Ptr SXMatrixRaw) -> CInt -> (Ptr SXMatrixRaw) -> CInt -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "&sxFunctionDelete" c_sxFunctionDelete :: FunPtr (Ptr SXFunctionRaw -> IO ())
foreign import ccall unsafe "sxFunctionGetNumInputs" c_sxFunctionGetNumInputs :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetNumOutputs" c_sxFunctionGetNumOutputs :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetInputs" c_sxFunctionGetInputs :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGetOutputs" c_sxFunctionGetOutputs :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGradient" c_sxFunctionGradient :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionJacobian" c_sxFunctionJacobian :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionHessian" c_sxFunctionHessian :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionEvaluate" c_sxFunctionEvaluate :: Ptr SXFunctionRaw -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "sxFunctionGetEvaluatedOutput" c_sxFunctionGetEvaluatedOutput :: Ptr SXFunctionRaw -> CInt -> CInt -> CInt -> Ptr CDouble -> IO ()


------------------ creation -------------------------
sxFunctionCreate' :: SXMatrix -> SXMatrix -> IO SXFunction
sxFunctionCreate' (SXMatrix m0) (SXMatrix m1) = mask_ $ do
  fun <- withForeignPtrs2 c_sxFunctionCreate m0 m1 >>= newForeignPtr c_sxFunctionDelete
  return $ SXFunction fun

sxFunctionCreate :: [SXMatrix] -> [SXMatrix] -> SXFunction
sxFunctionCreate mListIn mListOut = unsafePerformIO $ mask_ $ do
  (mArrayIn,  lArrayIn)  <- sxMatrixListToUnsafeArray mListIn
  (mArrayOut, lArrayOut) <- sxMatrixListToUnsafeArray mListOut

  let lArrayIn' = fromIntegral lArrayIn
      lArrayOut' = fromIntegral lArrayOut

  fun <- withForeignPtrs2 (\mIn mOut -> c_sxFunctionCreateMulti mIn lArrayIn' mOut lArrayOut') mArrayIn mArrayOut >>= newForeignPtr c_sxFunctionDelete

  sxMatrixFreeUnsafeArray mArrayIn lArrayIn
  sxMatrixFreeUnsafeArray mArrayOut lArrayOut

  return $ SXFunction fun


------------------- getters -----------------------
sxFunctionNumInputs :: SXFunction -> Int
sxFunctionNumInputs (SXFunction fun) = unsafePerformIO $ do
  num <- withForeignPtr fun c_sxFunctionGetNumInputs
  return $ fromIntegral num

sxFunctionNumOutputs :: SXFunction -> Int
sxFunctionNumOutputs (SXFunction fun) = unsafePerformIO $ do
  num <- withForeignPtr fun c_sxFunctionGetNumOutputs
  return $ fromIntegral num

sxFunctionGetInputs :: SXFunction -> Int -> SXMatrix
sxFunctionGetInputs (SXFunction fun) idx = unsafePerformIO $ do
  if idx >= sxFunctionNumInputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGetInputs - requested input index: %d >= numInputs (SXFunction fun): %d" idx (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetInputs fun' (fromIntegral idx) mat') fun mat
  return (SXMatrix mat)

sxFunctionGetOutputs :: SXFunction -> Int -> SXMatrix
sxFunctionGetOutputs (SXFunction fun) idx = unsafePerformIO $ do
  if idx >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGetOutputs - requested output index: %d >= numOutputs (SXFunction fun): %d" idx (sxFunctionNumOutputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetOutputs fun' (fromIntegral idx) mat') fun mat
  return (SXMatrix mat)


----------------------- AD -----------------------
sxFunctionGradientAt :: SXFunction -> Int -> SXMatrix
sxFunctionGradientAt (SXFunction fun) idxInput = unsafePerformIO $ do
  -- don't take gradient with respect to non-existant input
  if idxInput >= sxFunctionNumInputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGradientAt - requested gradient index: %d >= numInputs (SXFunction fun): %d" idxInput (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  -- don't take gradient of vector valued function
  if (1,1) /= (sxMatrixSize $ sxFunctionGetOutputs (SXFunction fun) 0)
    then error $ printf "Error in sxFunctionGradientAt - requested gradient of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGradient fun' (fromIntegral idxInput) mat') fun mat
  return $ (SXMatrix mat)


sxFunctionGradients :: SXFunction -> [SXMatrix]
sxFunctionGradients fun = map (sxFunctionGradientAt fun) $ take (sxFunctionNumInputs fun) [0..]


sxFunctionJacobianAt :: SXFunction -> (Int, Int) -> SXMatrix
sxFunctionJacobianAt (SXFunction fun) (idx0, idx1) = unsafePerformIO $ do
  -- don't take jacobian with respect to non-existant output
  if idx0 >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numOutputs (SXFunction fun): %d" idx0 idx1 (sxFunctionNumOutputs (SXFunction fun))
    else do return ()

  -- don't take jacobian with respect to non-existant input
  if idx1 >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numInputs (SXFunction fun): %d" idx0 idx1 (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionJacobian fun' (fromIntegral idx0) (fromIntegral idx1) mat') fun mat
  return $ (SXMatrix mat)


sxFunctionHessianAt :: SXFunction -> (Int, Int) -> SXMatrix
sxFunctionHessianAt (SXFunction fun) (idx0, idx1) = unsafePerformIO $ do
  -- don't take hessian with respect to non-existant input
  if any (\x -> x >= sxFunctionNumInputs (SXFunction fun)) [idx0, idx1]
    then error $ printf "Error in sxFunctionHessianAt - requested hessian index: (%d,%d) >= numInputs (SXFunction fun): %d" idx0 idx1 (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  -- don't take hessian of vector valued function
  if (1,1) /= (sxMatrixSize $ sxFunctionGetOutputs (SXFunction fun) 0)
    then error $ printf "Error in sxFunctionHessianAt - requested hessian of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionHessian fun' (fromIntegral idx0) (fromIntegral idx1) mat') fun mat
  return $ (SXMatrix mat)


--------------------- evaluate -----------------------------
sxFunctionUnsafeEvaluateInput :: SXFunction -> [[[Double]]] -> IO ()
sxFunctionUnsafeEvaluateInput (SXFunction fun) inputsList = do
  let numRows :: [[a]] -> CInt
      numRows mat = fromIntegral $ length mat
      numCols :: [[a]] -> CInt
      numCols mat = fromIntegral $ length (head mat)
      inputRows = map numRows inputsList
      inputCols = map numCols inputsList
      inputsCDouble = map realToFrac (concat $ map concat inputsList) :: [CDouble]
  inputsArray <- newArray inputsCDouble
  rowsArray <- newArray inputRows
  colsArray <- newArray inputCols

  withForeignPtr fun (\fun' -> c_sxFunctionEvaluate fun' inputsArray rowsArray colsArray)

sxFunctionUnsafeGetEvaluatedOutput :: SXFunction -> Int -> IO [[Double]]
sxFunctionUnsafeGetEvaluatedOutput (SXFunction fun) outputIndex = do
  let -- this might be slow:
      (rows, cols) = sxMatrixSize $ sxFunctionGetOutputs (SXFunction fun) outputIndex

  outputsArray <- mallocArray $ fromIntegral (rows*cols)
  let outputIndex' = fromIntegral outputIndex
      rows' = fromIntegral  rows
      cols' = fromIntegral cols
  withForeignPtr fun (\fun' -> c_sxFunctionGetEvaluatedOutput fun' outputIndex' rows' cols' outputsArray)

  outputsList <- peekArray (fromIntegral $ rows*cols) outputsArray

  let reshape' :: [[Double]] -> [Double] -> [[Double]]
      reshape'  acc [] = acc
      reshape' acc xs = reshape' (acc ++ [firstN]) (notFirstN)
        where
          (firstN, notFirstN) = splitAt (fromIntegral cols) xs

  return $ reshape' [] (map realToFrac outputsList)

sxFunctionEvaluate :: SXFunction -> [[[Double]]] -> IO [[[Double]]]
sxFunctionEvaluate fun input = do
  sxFunctionUnsafeEvaluateInput fun input
  mapM (sxFunctionUnsafeGetEvaluatedOutput fun) [0..(sxFunctionNumOutputs fun)-1]


--main :: IO ()
--main = do
--  a <- sxMatrixCreateSymbolic "a" (1,1)
--  b <- sxMatrixCreateSymbolic "b" (2,1)
--
--  let --(a0:a1:_) = sxMatrixToList a
--      --(b0:b1:b2:_) = sxMatrixToList b
--
--      (a0:_) = sxMatrixToList a
--      (b0:b1:_) = sxMatrixToList b
--
----      c = sxMatrixFromList [a0*b0 + a1*b1/b2]
--      c = sxMatrixFromList [2*a0*a0 + 7*b0*b0 + 8*b1*b1 + a0*b0*b1]
--
--      fun = sxFunctionCreate [a,b] [c]
----      fun = sxFunction [a,b] [c, []]
--      g = sxFunctionCreate [a,b] (sxFunctionGradients fun)
--
--      a' = [3::Double]
--      b' = [4,5::Double]
--
--  print [a,b]
--  print (sxFunctionGradients fun)
--  op <- sxFunctionEvaluate g [a',b']
--  print op
--
--
----  print $ sxFunctionJacobianAt g (0,0)
----  print $ sxFunctionJacobianAt g (0,1)
----  print $ sxFunctionJacobianAt g (1,0)
----  print $ sxFunctionJacobianAt g (1,1)
--
----  print $ sxFunctionHessianAt fun (0,0)
----  print $ sxFunctionHessianAt fun (0,1)
----  print $ sxFunctionHessianAt fun (1,0)
----  print $ sxFunctionHessianAt fun (1,1)
--
--
--
--  putStrLn "(end of SXFunction.main)"
