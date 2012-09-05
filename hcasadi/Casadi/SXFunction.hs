{-# OPTIONS_GHC -Wall #-}

module Casadi.SXFunction ( SXFunction(..)
                         , sxFunctionCreate
                         , sxFunctionNumInputs
                         , sxFunctionNumOutputs
                         , sxFunctionInputSize
                         , sxFunctionInputSize1
                         , sxFunctionInputSize2
                         , sxFunctionOutputSize
                         , sxFunctionOutputSize1
                         , sxFunctionOutputSize2

--                         , sxFunctionEvaluate
--                         , sxFunctionEvaluateLists
--                         , sxFunctionGetInputsSX
--                         , sxFunctionGetOutputsSX
--                         , sxFunctionGradientAt
--                         , sxFunctionGradients
--                         , sxFunctionJacobianAt
--                         , sxFunctionHessianAt
--                         , sxFunctionCompile
                         ) where

import Control.Applicative ( (<$>) )

import qualified Control.Exception(catch)
import Foreign.C
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Marshal(newArray)
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import System.IO
import Text.Printf
--import System.Process
import System.Exit(ExitCode(..))
import Debug.Trace
--import Data.Time.Clock

import Casadi.Bindings.SXM
import Casadi.Bindings.SXFunction
import Casadi.SXM
--import Casadi.DMatrix
--import Casadi.Matrix
import Casadi.CasadiInterfaceUtils


newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)

-- | create SXFunction from list of inputs and ouputs
sxFunctionCreate :: [SXM] -> [SXM] -> IO SXFunction
sxFunctionCreate inputs outputs = mask_ $ do
  -- turn input/output SXM lists into [Ptr SXMRaw]
  let unsafeInputPtrs :: [Ptr SXMRaw]
      unsafeInputPtrs = map (\(SXM mat) -> unsafeForeignPtrToPtr mat) inputs
      
      unsafeOutputPtrs :: [Ptr SXMRaw]
      unsafeOutputPtrs = map (\(SXM mat) -> unsafeForeignPtrToPtr mat) outputs
      
      nIn  = fromIntegral $ length inputs
      nOut = fromIntegral $ length outputs
  
  -- turn [Ptr SXMRaw] into Ptr (Ptr SXMRaw)
  inputPtrArray <- newArray unsafeInputPtrs
  outputPtrArray <- newArray unsafeOutputPtrs
  
  -- create SXFunction
  funRaw <- c_sxFunctionCreate inputPtrArray nIn outputPtrArray nOut  >>= newForeignPtr c_sxFunctionDelete
  
  -- touch all [ForeignPtr SXMRaw] for unsafeForeignPtrToPtr safety
  mapM_ (\(SXM d) -> touchForeignPtr d) inputs
  mapM_ (\(SXM d) -> touchForeignPtr d) outputs
  
  return $ SXFunction funRaw
  
--foreign import ccall unsafe "sxFunctionGetInputsSX" c_sxFunctionGetInputsSX
--  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionGetOutputsSX" c_sxFunctionGetOutputsSX
--  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionGradient" c_sxFunctionGradient
--  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionJacobian" c_sxFunctionJacobian
--  :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionHessian" c_sxFunctionHessian
--  :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMRaw -> IO ()
--
--foreign import ccall unsafe "generateCCode" c_generateCCode
--  :: Ptr CChar -> Ptr SXFunctionRaw -> IO CDouble
--foreign import ccall unsafe "createExternalFunction" c_createExternalFunction
--  :: Ptr CChar -> IO (Ptr SXFunctionRaw)




--------------------- getters -----------------------
--foreign import ccall unsafe "sxFunctionNumInputs" c_sxFunctionNumInputs
--  :: Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionNumOutputs" c_sxFunctionNumOutputs
--  :: Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionInputSize" c_sxFunctionInputSize
--  :: CInt -> Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionInputSize1" c_sxFunctionInputSize1
--  :: CInt -> Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionInputSize2" c_sxFunctionInputSize2
--  :: CInt -> Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionOutputSize" c_sxFunctionOutputSize
--  :: CInt -> Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionOutputSize1" c_sxFunctionOutputSize1
--  :: CInt -> Ptr SXFunctionRaw -> IO CInt
--foreign import ccall unsafe "sxFunctionOutputSize2" c_sxFunctionOutputSize2
--  :: CInt -> Ptr SXFunctionRaw -> IO CInt

sxFunctionNumInputs, sxFunctionNumOutputs :: SXFunction -> IO Int
sxFunctionNumInputs  (SXFunction f) = mask_ $ fromIntegral <$> withForeignPtr f c_sxFunctionNumInputs
sxFunctionNumOutputs (SXFunction f) = mask_ $ fromIntegral <$> withForeignPtr f c_sxFunctionNumOutputs

sxFunctionInputSize, sxFunctionInputSize1, sxFunctionInputSize2 :: SXFunction -> CInt -> IO Int
sxFunctionInputSize (SXFunction f) k = mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionInputSize k)
sxFunctionInputSize1 (SXFunction f) k = mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionInputSize1 k)
sxFunctionInputSize2 (SXFunction f) k = mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionInputSize2 k)

sxFunctionOutputSize, sxFunctionOutputSize1, sxFunctionOutputSize2 :: SXFunction -> CInt -> IO Int
sxFunctionOutputSize (SXFunction f) k = mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionOutputSize k)
sxFunctionOutputSize1 (SXFunction f) k = mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionOutputSize1 k)
sxFunctionOutputSize2 (SXFunction f) k = mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionOutputSize2 k)



--checkSXFunctionDimensions :: SXFunction -> IO ()
--checkSXFunctionDimensions fun = do
--                        
--  let sxFunctionNumInputs :: SXFunction -> IO Int
--      sxFunctionNumInputs fun' = do
--        num <- withForeignPtr (sxFunRaw fun') c_sxFunctionGetNumInputs
--        return $ fromIntegral num
--      
--      sxFunctionNumOutputs :: SXFunction -> IO Int
--      sxFunctionNumOutputs fun' = do
--        num <- withForeignPtr (sxFunRaw fun') c_sxFunctionGetNumOutputs
--        return $ fromIntegral num
--      
--      sxFunctionGetInputDim :: SXFunction -> Int -> IO (Int, Int)
--      sxFunctionGetInputDim fun' idx = do
--        size1 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetInputSize1 (fromIntegral idx)
--        size2 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetInputSize2 (fromIntegral idx)
--        return (fromIntegral size1, fromIntegral size2)
--      
--      sxFunctionGetOutputDim :: SXFunction -> Int -> IO (Int, Int)
--      sxFunctionGetOutputDim fun' idx = do
--        size1 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetOutputSize1 (fromIntegral idx)
--        size2 <- withForeignPtr (sxFunRaw fun') $ c_sxFunctionGetOutputSize2 (fromIntegral idx)
--        return (fromIntegral size1, fromIntegral size2)
--
--  numInputs <- sxFunctionNumInputs fun
--  numOutputs <- sxFunctionNumOutputs fun
--  inputDim <- mapM (sxFunctionGetInputDim fun) [0..numInputs - 1]
--  outputDim <- mapM (sxFunctionGetOutputDim fun) [0..numOutputs - 1]
--  
--  let ret
--        | numInputs  /= sxFunNumInputs fun  = error "checkSXFunctionDimensions got bad numInputs"
--        | numOutputs /= sxFunNumOutputs fun = error "checkSXFunctionDimensions got bad numOutputs"
--        | inputDim   /= sxFunInputDims fun  = error "checkSXFunctionDimensions got bad inputDim"
--        | outputDim  /= sxFunOutputDims fun = error "checkSXFunctionDimensions got bad outputDim"
--        | otherwise                         = ()
--  return $ ret `seq` ret
  
  
--sxFunctionGetInputsSX :: SXFunction -> Int -> SXM
--{-# NOINLINE sxFunctionGetInputsSX #-}
--sxFunctionGetInputsSX fun idx = trace "why are you using sxFunctionGetInputsSX?" $ unsafePerformIO $ do
--  if idx >= sxFunNumInputs fun
--    then error $ printf "Error in sxFunctionGetInputsSX - requested input index: %d >= numInputs (SXFunction fun): %d" idx (sxFunNumInputs fun)
--    else do return ()
--
--  SXM mat <- sxMatrixNewZeros (1::Int,1::Int)
--  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetInputsSX fun' (fromIntegral idx) mat') (sxFunRaw fun) mat
--  return (SXM mat)
--
--
--sxFunctionGetOutputsSX :: SXFunction -> Int -> SXM
--{-# NOINLINE sxFunctionGetOutputsSX #-}
--sxFunctionGetOutputsSX fun idx = trace "why are you using sxFunctionGetOutputsSX?" $ unsafePerformIO $ do
--  if idx >= sxFunNumOutputs fun
--    then error $ printf "Error in sxFunctionGetOutputsSX - requested output index: %d >= numOutputs (SXFunction fun): %d" idx (sxFunNumOutputs fun)
--    else do return ()
--
--  SXM mat <- sxMatrixNewZeros (1::Int,1::Int)
--  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGetOutputsSX fun' (fromIntegral idx) mat') (sxFunRaw fun) mat
--  return (SXM mat)


------------------------- AD -----------------------
--sxFunctionGradientAt :: SXFunction -> Int -> SXM
--{-# NOINLINE sxFunctionGradientAt #-}
--sxFunctionGradientAt fun idxInput = unsafePerformIO $ do
--  -- don't take gradient with respect to non-existant input
--  if idxInput >= sxFunNumInputs fun
--    then error $ printf "Error in sxFunctionGradientAt - requested gradient index: %d >= numInputs fun: %d" idxInput (sxFunNumInputs fun)
--    else do return ()
--
--  -- don't take gradient of vector valued function
--  if (1,1) /= head (sxFunOutputDims fun)
--    then error $ printf "Error in sxFunctionGradientAt - requested gradient of non-scalar"
--    else do return ()
--
--  SXM mat <- sxMatrixNewZeros (1::Int,1::Int)
--  withForeignPtrs2 (\fun' mat' -> c_sxFunctionGradient fun' (fromIntegral idxInput) mat') (sxFunRaw fun) mat
--  return $ (SXM mat)
--
--
--sxFunctionGradients :: SXFunction -> [SXM]
--sxFunctionGradients fun = map (sxFunctionGradientAt fun) $ take (sxFunNumInputs fun) [0..]
--
--
--sxFunctionJacobianAt :: SXFunction -> (Int, Int) -> SXM
--{-# NOINLINE sxFunctionJacobianAt #-}
--sxFunctionJacobianAt fun (idx0, idx1) = unsafePerformIO $ do
--  -- don't take jacobian with respect to non-existant output
--  if idx0 >= sxFunNumOutputs fun
--    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numOutputs fun: %d" idx0 idx1 (sxFunNumOutputs fun)
--    else do return ()
--
--  -- don't take jacobian with respect to non-existant input
--  if idx1 >= sxFunNumOutputs fun
--    then error $ printf "Error in sxFunctionJacobianAt - requested jacobian index: (%d,%d) is outside numInputs fun: %d" idx0 idx1 (sxFunNumInputs fun)
--    else do return ()
--
--  SXM mat <- sxMatrixNewZeros (1::Int,1::Int)
--  withForeignPtrs2 (\fun' mat' -> c_sxFunctionJacobian fun' (fromIntegral idx0) (fromIntegral idx1) mat') (sxFunRaw fun) mat
--  return $ (SXM mat)
--
--
--sxFunctionHessianAt :: SXFunction -> (Int, Int) -> SXM
--{-# NOINLINE sxFunctionHessianAt #-}
--sxFunctionHessianAt fun (idx0, idx1) = unsafePerformIO $ do
--  -- don't take hessian with respect to non-existant input
--  if any (\x -> x >= sxFunNumInputs fun) [idx0, idx1]
--    then error $ printf "Error in sxFunctionHessianAt - requested hessian index: (%d,%d) >= numInputs fun: %d" idx0 idx1 (sxFunNumInputs fun)
--    else do return ()
--
--  -- don't take hessian of vector valued function
--  if (1,1) /= head (sxFunOutputDims fun)
--    then error $ printf "Error in sxFunctionHessianAt - requested hessian of non-scalar"
--    else do return ()
--
--  SXM mat <- sxMatrixNewZeros (1::Int,1::Int)
--  withForeignPtrs2 (\fun' mat' -> c_sxFunctionHessian fun' (fromIntegral idx0) (fromIntegral idx1) mat') (sxFunRaw fun) mat
--  return $ (SXM mat)


------------------------- evaluate -----------------------------
--sxFunctionEvaluate :: SXFunction -> [(Ptr Double, Int)] -> IO [(Ptr Double,Int)]
--sxFunctionEvaluate fun inputs = do
--  let inputPtrs :: [Ptr Double]
--      inputPtrs = map fst inputs
--
--  outputs <- mapM newZeros (sxFunOutputDims fun)
--  let unsafeOutputPtrs :: [Ptr Double]
--      unsafeOutputPtrs = map (\m -> unsafeForeignPtrToPtr (getForeignPtr m)) outputs
--  
--  inputPtrArray <- newArray unsafeInputPtrs
--  outputPtrArray <- newArray unsafeOutputPtrs
--  
--  let nIn = fromIntegral $ length inputs
--      nOut = fromIntegral$ length outputs
--
--  let eval :: CInt -> Ptr (Ptr c) -> CInt -> Ptr (Ptr c) -> Ptr SXFunctionRaw -> IO ()
--      eval = c_sxFunctionEvaluate (head inputs)
--  withForeignPtr (sxFunRaw fun) (eval nIn inputPtrArray nOut outputPtrArray)
--  
--  mapM_ (\d -> touchForeignPtr (getForeignPtr d)) inputs
--  mapM_ (\d -> touchForeignPtr (getForeignPtr d)) outputs
--  
--  return outputs


--sxFunctionEvaluateLists :: SXFunction -> [[[Double]]] -> [[[Double]]]
--{-# NOINLINE sxFunctionEvaluateLists #-}
--sxFunctionEvaluateLists fun inputs = unsafePerformIO $ do
--  let outNew = map toLists $ sxFunctionEvaluate fun $ (map fromLists inputs :: [DMatrix])
--  return outNew
--
--
--getMd5 :: String -> IO String
--getMd5 filename = do
--  (_, hStdout, _, p) <- runInteractiveCommand $ "md5sum " ++ filename
--  exitCode <- waitForProcess p
--  md5Out <- hGetContents hStdout
--  if exitCode == ExitSuccess
--    then do return $ head (lines md5Out)
--    else do error $ "getMd5 couldn't read \"" ++ filename ++ "\""
--
------------------------ code gen ---------------------
--sxFunctionCompile :: SXFunction -> String -> ([DMatrix] -> [DMatrix])
--sxFunctionCompile fun name = unsafePerformIO $ do
--  
--  let srcname = name ++ ".c"
--      objname  = name ++ ".so"
--      hashname  = name ++ ".so.md5"
--
--  cSrcname <- newCString srcname
--  cObjname  <- newCString objname
--
--  let funPtr = unsafeForeignPtrToPtr (sxFunRaw fun)
--
--  -- generate code
--  genTime <- c_generateCCode cSrcname funPtr
--  touchForeignPtr (sxFunRaw fun)
--  putStrLn $ "Generated " ++ srcname ++ " in " ++ show (realToFrac genTime::Double) ++ " seconds"
--
--  -- check md5
--  let getOldMd5 = do Control.Exception.catch (readFile ("./" ++ hashname)) $ \(_::IOError) -> do return $ hashname ++ " does not exist"
--  
--  oldMd5 <- getOldMd5
--  newMd5 <- getMd5 srcname
--  putStrLn $ "oldMd5: \"" ++ oldMd5 ++ "\""
--  putStrLn $ "newMd5: \"" ++ newMd5 ++ "\""
--  
--  if oldMd5 /= newMd5
--    -- compile new object
--    then do let compileString = "gcc -O1 -fPIC -shared " ++ srcname ++ " -o " ++ objname
--            putStrLn compileString
--            p <- runCommand compileString
--            exitCode <- timeComputation "compiled in " $ waitForProcess p
--            if exitCode /= ExitSuccess
--              then do error "compilation failure"
--              else do writeFile ("./" ++ hashname) newMd5
--                      return ()
--    -- use old object
--    else do putStrLn $ "md5 of " ++ srcname ++ " matches " ++ hashname ++ " - reusing " ++ objname
--
--  extFun <- c_createExternalFunction cObjname >>= newForeignPtr c_sxFunctionDelete
--              
--  return $ sxFunctionEvaluate $ SXFunction { sxFunRaw = extFun
--                                           , sxFunNumInputs  = sxFunNumInputs fun
--                                           , sxFunNumOutputs = sxFunNumOutputs fun
--                                           , sxFunInputDims  = sxFunInputDims  fun
--                                           , sxFunOutputDims = sxFunOutputDims fun}
--
--timeComputation :: String -> IO t -> IO t
--timeComputation msg a = do
--    start <- getCurrentTime
--    v <- a
--    end   <- getCurrentTime
--    let diffTime = (realToFrac $ diffUTCTime end start)::Double
--    putStrLn $ msg ++ show diffTime ++ " seconds"
--    return v
