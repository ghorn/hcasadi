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
       , sxFunctionEvaluateLists
       , sxFunctionEvaluateListsOld
       , sxFunctionGetInputsSX
       , sxFunctionGetOutputsSX
       , sxFunctionGetInputDim
       , sxFunctionGetOutputDim
       , sxFunctionGradientAt
       , sxFunctionGradients
       , sxFunctionJacobianAt
       , sxFunctionHessianAt
       ) where

import Casadi.DMatrix
import Casadi.SXMatrix
import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal(newArray, mallocArray, peekArray)
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Text.Printf
import Control.DeepSeq


-- the SXFunction data type
data SXFunctionRaw = SXFunctionRaw
newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)

-- foreign imports
foreign import ccall unsafe "sxFunctionCreate" c_sxFunctionCreate :: Ptr SXMatrixRaw -> Ptr SXMatrixRaw -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "sxFunctionCreateMulti" c_sxFunctionCreateMulti :: (Ptr SXMatrixRaw) -> CInt -> (Ptr SXMatrixRaw) -> CInt -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "&sxFunctionDelete" c_sxFunctionDelete :: FunPtr (Ptr SXFunctionRaw -> IO ())
foreign import ccall unsafe "sxFunctionGetNumInputs" c_sxFunctionGetNumInputs :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetNumOutputs" c_sxFunctionGetNumOutputs :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetInputsSX" c_sxFunctionGetInputsSX :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGetOutputsSX" c_sxFunctionGetOutputsSX :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGetInputSize1" c_sxFunctionGetInputSize1 :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetInputSize2" c_sxFunctionGetInputSize2 :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetOutputSize1" c_sxFunctionGetOutputSize1 :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGetOutputSize2" c_sxFunctionGetOutputSize2 :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunctionGradient" c_sxFunctionGradient :: Ptr SXFunctionRaw -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionJacobian" c_sxFunctionJacobian :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionHessian" c_sxFunctionHessian :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionEvaluateInputOld" c_sxFunctionEvaluateInputOld :: Ptr SXFunctionRaw -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "sxFunctionGetEvaluatedOutputOld" c_sxFunctionGetEvaluatedOutputOld :: Ptr SXFunctionRaw -> CInt -> CInt -> CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "sxFunctionEvaluate" c_sxFunctionEvaluate :: Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunctionSetInput" c_sxFunctionSetInput :: CInt -> Ptr SXFunctionRaw -> Ptr DMatrixRaw -> IO ()
foreign import ccall unsafe "sxFunctionGetEvaluatedOutput" c_sxFunctionGetEvaluatedOutput :: CInt -> Ptr SXFunctionRaw -> Ptr DMatrixRaw -> IO ()


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

sxFunctionGetInputsSX :: SXFunction -> Int -> SXMatrix
sxFunctionGetInputsSX (SXFunction fun) idx = unsafePerformIO $ do
  if idx >= sxFunctionNumInputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGetInputsSX - requested input index: %d >= numInputs (SXFunction fun): %d" idx (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetInputsSX fun' (fromIntegral idx) mat') fun mat
  return (SXMatrix mat)

sxFunctionGetOutputsSX :: SXFunction -> Int -> SXMatrix
sxFunctionGetOutputsSX (SXFunction fun) idx = unsafePerformIO $ do
  if idx >= sxFunctionNumOutputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGetOutputsSX - requested output index: %d >= numOutputs (SXFunction fun): %d" idx (sxFunctionNumOutputs (SXFunction fun))
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetOutputsSX fun' (fromIntegral idx) mat') fun mat
  return (SXMatrix mat)

sxFunctionGetInputDim :: SXFunction -> Int -> (Int, Int)
sxFunctionGetInputDim (SXFunction fun) idx = unsafePerformIO $ do
  size1 <- withForeignPtr fun $ c_sxFunctionGetInputSize1 (fromIntegral idx)
  size2 <- withForeignPtr fun $ c_sxFunctionGetInputSize2 (fromIntegral idx)
  return (fromIntegral size1, fromIntegral size2)

sxFunctionGetOutputDim :: SXFunction -> Int -> (Int, Int)
sxFunctionGetOutputDim (SXFunction fun) idx = unsafePerformIO $ do
  size1 <- withForeignPtr fun $ c_sxFunctionGetOutputSize1 (fromIntegral idx)
  size2 <- withForeignPtr fun $ c_sxFunctionGetOutputSize2 (fromIntegral idx)
  return (fromIntegral size1, fromIntegral size2)

----------------------- AD -----------------------
sxFunctionGradientAt :: SXFunction -> Int -> SXMatrix
sxFunctionGradientAt (SXFunction fun) idxInput = unsafePerformIO $ do
  -- don't take gradient with respect to non-existant input
  if idxInput >= sxFunctionNumInputs (SXFunction fun)
    then error $ printf "Error in sxFunctionGradientAt - requested gradient index: %d >= numInputs (SXFunction fun): %d" idxInput (sxFunctionNumInputs (SXFunction fun))
    else do return ()

  -- don't take gradient of vector valued function
  if (1,1) /= (sxMatrixSize $ sxFunctionGetOutputsSX (SXFunction fun) 0)
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
  if (1,1) /= (sxMatrixSize $ sxFunctionGetOutputsSX (SXFunction fun) 0)
    then error $ printf "Error in sxFunctionHessianAt - requested hessian of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionHessian fun' (fromIntegral idx0) (fromIntegral idx1) mat') fun mat
  return $ (SXMatrix mat)


--------------------- evaluate -----------------------------
sxFunctionUnsafeEvaluateInputOld :: SXFunction -> [[[Double]]] -> IO ()
sxFunctionUnsafeEvaluateInputOld (SXFunction fun) inputsList = do
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
  
  withForeignPtr fun (\fun' -> c_sxFunctionEvaluateInputOld fun' inputsArray rowsArray colsArray)


sxFunctionUnsafeGetEvaluatedOutputOld :: SXFunction -> Int -> IO [[Double]]
sxFunctionUnsafeGetEvaluatedOutputOld (SXFunction fun) outputIndex = do
  let (rows, cols) = sxFunctionGetOutputDim (SXFunction fun) outputIndex

  outputsArray <- mallocArray $ fromIntegral (rows*cols)
  let outputIndex' = fromIntegral outputIndex
      rows' = fromIntegral  rows
      cols' = fromIntegral cols
  withForeignPtr fun (\fun' -> c_sxFunctionGetEvaluatedOutputOld fun' outputIndex' rows' cols' outputsArray)

  outputsList <- peekArray (fromIntegral $ rows*cols) outputsArray

  let reshape' :: [[Double]] -> [Double] -> [[Double]]
      reshape'  acc [] = acc
      reshape' acc xs = reshape' (acc ++ [firstN]) (notFirstN)
        where
          (firstN, notFirstN) = splitAt (fromIntegral cols) xs

  return $ reshape' [] (map realToFrac outputsList)

sxFunctionEvaluateListsOld :: SXFunction -> [[[Double]]] -> IO [[[Double]]]
sxFunctionEvaluateListsOld fun input = do
--  putStrLn "sxFunctionEvaluateOld called !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  sxFunctionUnsafeEvaluateInputOld fun input
  ret <- mapM (sxFunctionUnsafeGetEvaluatedOutputOld fun) [0..(sxFunctionNumOutputs fun)-1]
  return ret

sxFunctionEvaluateLists :: SXFunction -> [[[Double]]] -> [[[Double]]]
sxFunctionEvaluateLists fun inputs = unsafePerformIO $ do
  let outNew = map dMatrixToLists $ sxFunctionEvaluate fun $ map dMatrixFromLists inputs

--  outOld <- sxFunctionEvaluateOld fun inputs

--  if outNew /= outOld
--    then do error "outputs don't match"
--    else do return $ outNew

  return outNew


sxFunctionEvaluate :: SXFunction -> [DMatrix] -> [DMatrix]
sxFunctionEvaluate fun@(SXFunction funRaw) inputs = unsafePerformIO $ do
--  putStrLn "\n\n=========================== sxFunctionEvaluateDMatrix called =============================="
--  putStrLn "sxFunctionEvaluateDMatrix inputs: "
  inputs `deepseq` mapM_ (\(k,mIn) -> sxFunctionUnsafeSetInput fun k mIn) $ zip [0..] inputs
  
--  putStrLn "calling c_sxFunctionEvaluate"
  withForeignPtr funRaw c_sxFunctionEvaluate

--  putStrLn "calling sxFunctionGetUnsafeOutput"
  output <- mapM (sxFunctionUnsafeGetOutput fun) [0..(sxFunctionNumOutputs fun)-1]

  return output


sxFunctionUnsafeSetInput :: SXFunction -> Int -> DMatrix -> IO ()
sxFunctionUnsafeSetInput (SXFunction fun) idx (DMatrix input) = do
--  putStrLn $ "sxFunctionUnsafeSetInput idx: " ++ (show idx)
  withForeignPtrs2 (c_sxFunctionSetInput (fromIntegral idx)) fun input
  return ()


sxFunctionUnsafeGetOutput :: SXFunction -> Int -> IO DMatrix
sxFunctionUnsafeGetOutput (SXFunction fun) idx = do 
--  putStrLn $ "sxFunctionUnsafeGetOutput          idx: " ++ (show idx)
  DMatrix mOut <- dMatrixZeros (sxFunctionGetOutputDim (SXFunction fun) idx)
  withForeignPtrs2 (c_sxFunctionGetEvaluatedOutput (fromIntegral idx)) fun mOut
  return $ DMatrix mOut
