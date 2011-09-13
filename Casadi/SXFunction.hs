-- SXFunction.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Casadi.SXFunction
       (
         SXFunction(..)
       , sxFunctionCreate
       , sxFunctionEvaluate
       , sxFunctionEvaluateLists
       , sxFunctionGetInputsSX
       , sxFunctionGetOutputsSX
       , sxFunctionGradientAt
       , sxFunctionGradients
       , sxFunctionJacobianAt
       , sxFunctionHessianAt
       , sxFunctionCompile
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
import System.IO
import Text.Printf
import Control.DeepSeq
import System.Process
import System.Exit(ExitCode(..))
import Debug.Trace
import Data.Time.Clock

-- the SXFunction data type
data SXFunction = SXFunction { sxFunRaw :: ForeignPtr SXFunctionRaw
                             , sxFunNumInputs :: Int
                             , sxFunNumOutputs :: Int
                             , sxFunInputDims :: [(Int,Int)]
                             , sxFunOutputDims :: [(Int,Int)]
                             }

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

foreign import ccall unsafe "generateCCode" c_generateCCode
  :: Ptr CChar -> Ptr SXFunctionRaw -> IO CDouble
foreign import ccall unsafe "createExternalFunction" c_createExternalFunction
  :: Ptr CChar -> IO (Ptr SXFunctionRaw)


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
  
  -- prepare output data structure
  let funOut = SXFunction { sxFunRaw = funRaw
                          , sxFunNumInputs = length inputs
                          , sxFunNumOutputs = length outputs
                          , sxFunInputDims = map size inputs
                          , sxFunOutputDims = map size outputs}
  
  -- make sure dimensions are right
  checkSXFunctionDimensions funOut
  
  return funOut


------------------- getters -----------------------
checkSXFunctionDimensions :: SXFunction -> IO ()
checkSXFunctionDimensions fun = do
                        
  let sxFunctionNumInputs :: SXFunction -> IO Int
      sxFunctionNumInputs fun' = do
        num <- withForeignPtr (sxFunRaw fun') c_sxFunctionGetNumInputs
        return $ fromIntegral num
      
      sxFunctionNumOutputs :: SXFunction -> IO Int
      sxFunctionNumOutputs fun' = do
        num <- withForeignPtr (sxFunRaw fun') c_sxFunctionGetNumOutputs
        return $ fromIntegral num
      
      sxFunctionGetInputDim :: SXFunction -> Int -> IO (Int, Int)
      sxFunctionGetInputDim fun' idx = do
        size1 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetInputSize1 (fromIntegral idx)
        size2 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetInputSize2 (fromIntegral idx)
        return (fromIntegral size1, fromIntegral size2)
      
      sxFunctionGetOutputDim :: SXFunction -> Int -> IO (Int, Int)
      sxFunctionGetOutputDim fun' idx = do
        size1 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetOutputSize1 (fromIntegral idx)
        size2 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetOutputSize2 (fromIntegral idx)
        return (fromIntegral size1, fromIntegral size2)

  numInputs <- sxFunctionNumInputs fun
  numOutputs <- sxFunctionNumOutputs fun
  inputDim <- mapM (sxFunctionGetInputDim fun) [0..numInputs - 1]
  outputDim <- mapM (sxFunctionGetOutputDim fun) [0..numOutputs - 1]
  
  let ret
        | numInputs  /= sxFunNumInputs fun  = error "checkSXFunctionDimensions got bad numInputs"
        | numOutputs /= sxFunNumOutputs fun = error "checkSXFunctionDimensions got bad numOutputs"
        | inputDim   /= sxFunInputDims fun  = error "checkSXFunctionDimensions got bad inputDim"
        | outputDim  /= sxFunOutputDims fun = error "checkSXFunctionDimensions got bad outputDim"
        | otherwise                         = ()
  return $ ret `seq` ret
  
  
sxFunctionGetInputsSX :: SXFunction -> Int -> SXMatrix
{-# NOINLINE sxFunctionGetInputsSX #-}
sxFunctionGetInputsSX fun idx = trace "why are you using sxFunctionGetInputsSX?" $ unsafePerformIO $ do
  if idx >= sxFunNumInputs fun
    then error $ printf "Error in sxFunctionGetInputsSX - requested input index: %d >= numInputs (SXFunction fun): %d" idx (sxFunNumInputs fun)
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetInputsSX fun' (fromIntegral idx) mat') (sxFunRaw fun) mat
  return (SXMatrix mat)


sxFunctionGetOutputsSX :: SXFunction -> Int -> SXMatrix
{-# NOINLINE sxFunctionGetOutputsSX #-}
sxFunctionGetOutputsSX fun idx = trace "why are you using sxFunctionGetOutputsSX?" $ unsafePerformIO $ do
  if idx >= sxFunNumOutputs fun
    then error $ printf "Error in sxFunctionGetOutputsSX - requested output index: %d >= numOutputs (SXFunction fun): %d" idx (sxFunNumOutputs fun)
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetOutputsSX fun' (fromIntegral idx) mat') (sxFunRaw fun) mat
  return (SXMatrix mat)


----------------------- AD -----------------------
sxFunctionGradientAt :: SXFunction -> Int -> SXMatrix
{-# NOINLINE sxFunctionGradientAt #-}
sxFunctionGradientAt fun idxInput = unsafePerformIO $ do
  -- don't take gradient with respect to non-existant input
  if idxInput >= sxFunNumInputs fun
    then error $ printf "Error in sxFunctionGradientAt - requested gradient index: %d >= numInputs fun: %d" idxInput (sxFunNumInputs fun)
    else do return ()

  -- don't take gradient of vector valued function
  if (1,1) /= head (sxFunOutputDims fun)
    then error $ printf "Error in sxFunctionGradientAt - requested gradient of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGradient fun' (fromIntegral idxInput) mat') (sxFunRaw fun) mat
  return $ (SXMatrix mat)


sxFunctionGradients :: SXFunction -> [SXMatrix]
sxFunctionGradients fun = map (sxFunctionGradientAt fun) $ take (sxFunNumInputs fun) [0..]


sxFunctionJacobianAt :: SXFunction -> (Int, Int) -> SXMatrix
{-# NOINLINE sxFunctionJacobianAt #-}
sxFunctionJacobianAt fun (idx0, idx1) = unsafePerformIO $ do
  -- don't take jacobian with respect to non-existant output
  if idx0 >= sxFunNumOutputs fun
    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numOutputs fun: %d" idx0 idx1 (sxFunNumOutputs fun)
    else do return ()

  -- don't take jacobian with respect to non-existant input
  if idx1 >= sxFunNumOutputs fun
    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numInputs fun: %d" idx0 idx1 (sxFunNumInputs fun)
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionJacobian fun' (fromIntegral idx0) (fromIntegral idx1) mat') (sxFunRaw fun) mat
  return $ (SXMatrix mat)


sxFunctionHessianAt :: SXFunction -> (Int, Int) -> SXMatrix
{-# NOINLINE sxFunctionHessianAt #-}
sxFunctionHessianAt fun (idx0, idx1) = unsafePerformIO $ do
  -- don't take hessian with respect to non-existant input
  if any (\x -> x >= sxFunNumInputs fun) [idx0, idx1]
    then error $ printf "Error in sxFunctionHessianAt - requested hessian index: (%d,%d) >= numInputs fun: %d" idx0 idx1 (sxFunNumInputs fun)
    else do return ()

  -- don't take hessian of vector valued function
  if (1,1) /= head (sxFunOutputDims fun)
    then error $ printf "Error in sxFunctionHessianAt - requested hessian of non-scalar"
    else do return ()

  SXMatrix mat <- sxMatrixNewZeros (1::Int,1::Int)
  withForeignPtrs2 (\fun' mat' -> c_sxFunctionHessian fun' (fromIntegral idx0) (fromIntegral idx1) mat') (sxFunRaw fun) mat
  return $ (SXMatrix mat)


--------------------- evaluate -----------------------------
sxFunctionEvaluate :: forall a b c. Matrix a b c => SXFunction -> [a] -> [a]
{-# NOINLINE sxFunctionEvaluate #-}
sxFunctionEvaluate fun inputs = unsafePerformIO $ do
  do if (map size inputs) == sxFunInputDims fun
       then do return ()
       else error "sxFunctionEvaluate got bad input dimensions"
  
  let unsafeInputPtrs :: [Ptr c]
      unsafeInputPtrs = map (\m -> unsafeForeignPtrToPtr (getForeignPtr m)) inputs

  outputs <- mapM newZeros (sxFunOutputDims fun)
  let unsafeOutputPtrs :: [Ptr c]
      unsafeOutputPtrs = map (\m -> unsafeForeignPtrToPtr (getForeignPtr m)) outputs
  
  inputPtrArray <- newArray unsafeInputPtrs
  outputPtrArray <- newArray unsafeOutputPtrs
  
  let nIn = fromIntegral $ length inputs
      nOut = fromIntegral$ length outputs

  let eval :: CInt -> Ptr (Ptr c) -> CInt -> Ptr (Ptr c) -> Ptr SXFunctionRaw -> IO ()
      eval = c_sxFunctionEvaluate (head inputs)
  withForeignPtr (sxFunRaw fun) (eval nIn inputPtrArray nOut outputPtrArray)
  
  mapM_ (\d -> touchForeignPtr (getForeignPtr d)) inputs
  mapM_ (\d -> touchForeignPtr (getForeignPtr d)) outputs
  
  return outputs


sxFunctionEvaluateLists :: SXFunction -> [[[Double]]] -> [[[Double]]]
{-# NOINLINE sxFunctionEvaluateLists #-}
sxFunctionEvaluateLists fun inputs = unsafePerformIO $ do
  let outNew = map toLists $ sxFunctionEvaluate fun $ (map fromLists inputs :: [DMatrix])
  return outNew


getMd5 :: String -> IO String
getMd5 filename = do
  (_, hStdout, _, p) <- runInteractiveCommand $ "md5sum " ++ filename
  exitCode <- waitForProcess p
  md5Out <- hGetContents hStdout
  if exitCode == ExitSuccess
    then do return $ head (lines md5Out)
    else do error $ "getMd5 couldn't read \"" ++ filename ++ "\""

---------------------- code gen ---------------------
sxFunctionCompile :: SXFunction -> String -> ([DMatrix] -> [DMatrix])
sxFunctionCompile fun name = unsafePerformIO $ do
  
  let srcname = name ++ ".c"
      objname  = name ++ ".so"
      hashname  = name ++ ".so.md5"

  cSrcname <- newCString srcname
  cObjname  <- newCString objname

  let funPtr = unsafeForeignPtrToPtr (sxFunRaw fun)

  -- generate code
  genTime <- c_generateCCode cSrcname funPtr
  touchForeignPtr (sxFunRaw fun)
  putStrLn $ "Generated " ++ srcname ++ " in " ++ show (realToFrac genTime::Double) ++ " seconds"

  -- check md5
  let getOldMd5 = do catch (readFile ("./" ++ hashname)) $ \_ -> do return $ hashname ++ " does not exist"
  
  oldMd5 <- getOldMd5
  newMd5 <- getMd5 srcname
  putStrLn $ "oldMd5: \"" ++ oldMd5 ++ "\""
  putStrLn $ "newMd5: \"" ++ newMd5 ++ "\""
  
  if oldMd5 /= newMd5
    -- compile new object
    then do let compileString = "gcc -fPIC -shared " ++ srcname ++ " -o " ++ objname
            putStrLn compileString
            p <- runCommand compileString
            exitCode <- timeComputation "compiled in " $ waitForProcess p
            if exitCode /= ExitSuccess
              then do error "compilation failure"
              else do writeFile ("./" ++ hashname) newMd5
                      return ()
    -- use old object
    else do putStrLn $ "md5 of " ++ srcname ++ " matches " ++ hashname ++ " - reusing " ++ objname

  extFun <- c_createExternalFunction cObjname >>= newForeignPtr c_sxFunctionDelete
              
  return $ sxFunctionEvaluate $ SXFunction { sxFunRaw = extFun
                                           , sxFunNumInputs  = sxFunNumInputs fun
                                           , sxFunNumOutputs = sxFunNumOutputs fun
                                           , sxFunInputDims  = sxFunInputDims  fun
                                           , sxFunOutputDims = sxFunOutputDims fun}

timeComputation :: String -> IO t -> IO t
timeComputation msg a = do
    start <- getCurrentTime
    v <- a
    end   <- getCurrentTime
    let diffTime = (realToFrac $ diffUTCTime end start)::Double
    putStrLn $ msg ++ show diffTime ++ " seconds"
    return v
