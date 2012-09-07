{-# OPTIONS_GHC -Wall #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module Casadi.SXFunction ( SXFunction(..)
                         , SXFunctionOption
                         , sxFunctionCreate
                         , sxFunctionNumInputs
                         , sxFunctionNumOutputs
                         , sxFunctionInputSize
                         , sxFunctionInputSize1
                         , sxFunctionInputSize2
                         , sxFunctionOutputSize
                         , sxFunctionOutputSize1
                         , sxFunctionOutputSize2
                         , sxFunctionUnsafeSetInput
                         , sxFunctionUnsafeGetOutput
                         , sxFunctionUnsafeEval
                         , sxFunctionEval
                         , sxFunctionUnsafeSetOption
--                         , sxFunctionCompile
                         ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( mask_ )
import Data.Vector.Storable ( Vector )
import qualified Data.Vector.Storable as V
import Foreign.C ( CDouble(..), newCString )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal ( newArray, mallocArray, free, finalizerFree )
--import System.IO.Unsafe(unsafePerformIO)
--import System.IO
--import Text.Printf
--import System.Process
--import System.Exit(ExitCode(..))
--import Debug.Trace
--import Data.Time.Clock

import Casadi.Bindings.SXM
import Casadi.Bindings.SXFunction
import Casadi.WithForeignPtrs
import Casadi.SXM

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
  
--------------------- getters -----------------------
sxFunctionNumInputs, sxFunctionNumOutputs :: SXFunction -> IO Int
sxFunctionNumInputs  (SXFunction f) = mask_ $ fromIntegral <$> withForeignPtr f c_sxFunctionNumInputs
sxFunctionNumOutputs (SXFunction f) = mask_ $ fromIntegral <$> withForeignPtr f c_sxFunctionNumOutputs

sxFunctionInputSize, sxFunctionInputSize1, sxFunctionInputSize2 :: SXFunction -> Int -> IO Int
sxFunctionInputSize  (SXFunction f) k =
  mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionInputSize (fromIntegral k))
sxFunctionInputSize1 (SXFunction f) k =
  mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionInputSize1 (fromIntegral k))
sxFunctionInputSize2 (SXFunction f) k =
  mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionInputSize2 (fromIntegral k))

sxFunctionOutputSize, sxFunctionOutputSize1, sxFunctionOutputSize2 :: SXFunction -> Int -> IO Int
sxFunctionOutputSize  (SXFunction f) k =
  mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionOutputSize  (fromIntegral k))
sxFunctionOutputSize1 (SXFunction f) k =
  mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionOutputSize1 (fromIntegral k))
sxFunctionOutputSize2 (SXFunction f) k =
  mask_ $ fromIntegral <$> withForeignPtr f (c_sxFunctionOutputSize2 (fromIntegral k))

-- | tries to get output, returns Nothing on success or Just k on failure where
--   k is the __expected__ size of the output
sxFunctionUnsafeGetOutput :: SXFunction -> Int -> (ForeignPtr CDouble, Int) -> IO (Maybe Int)
sxFunctionUnsafeGetOutput (SXFunction fun) idx (val, valN) = do
  ret <- withForeignPtrs2 val fun (c_sxFunctionGetOutput (fromIntegral idx) (fromIntegral valN))
  return $ case ret of (-1) -> Nothing
                       n -> Just (fromIntegral n)

--------------------- setters -----------------------
-- | tries to set input, returns Nothing on success or Just k on failure where
--   k is the __expected__ size of the input
sxFunctionUnsafeSetInput :: SXFunction -> Int -> Vector CDouble -> IO (Maybe Int)
sxFunctionUnsafeSetInput (SXFunction fun) idx val' = do
  let (val, valN) = V.unsafeToForeignPtr0 val'
  ret <- withForeignPtrs2 val fun (c_sxFunctionSetInput (fromIntegral idx) (fromIntegral valN))
  return $ case ret of (-1) -> Nothing
                       n -> Just (fromIntegral n)
                       

------------------------- evaluate -----------------------------
-- | provide pointers to input data and output data (along with the number of doubles in each)
sxFunctionUnsafeEval :: SXFunction -> [(Ptr CDouble, Int)] -> [(Ptr CDouble, Int)] -> IO (Maybe Int)
sxFunctionUnsafeEval (SXFunction rawFun) inputs outputs = do
  let (inputPtrs,inputSizes) = unzip inputs
      numInputs = length inputs

      (outputPtrs,outputSizes) = unzip outputs
      numOutputs = length outputs

  inputPtrArray  <- newArray inputPtrs
  outputPtrArray <- newArray outputPtrs

  inputSizesArray  <- newArray (map fromIntegral inputSizes)
  outputSizesArray <- newArray (map fromIntegral outputSizes)

  ret <- withForeignPtr rawFun $ c_sxFunctionEvalDouble
         (fromIntegral numInputs)   inputPtrArray  inputSizesArray
         (fromIntegral numOutputs) outputPtrArray outputSizesArray

  free inputPtrArray
  free outputPtrArray

  free inputSizesArray
  free outputSizesArray

  return $ case ret of
    0 -> Nothing
    n -> Just (fromIntegral n)

sxFunctionEval :: SXFunction -> [(Ptr CDouble, Int)] -> IO [(ForeignPtr CDouble, Int)]
sxFunctionEval fun inputs = do
  numOutputs <- sxFunctionNumOutputs fun
  outputSizes <- mapM (sxFunctionOutputSize fun) (take numOutputs [0..])
  outputPtrs <- mapM mallocArray outputSizes
  ret <- sxFunctionUnsafeEval fun inputs (zip outputPtrs outputSizes)
  outputs <- mapM (newForeignPtr finalizerFree) outputPtrs

  return $ case ret of
    Nothing -> zip outputs outputSizes
    Just n -> error $ "sxFunctionUnsafeEval returned error code " ++ show n

class SXFunctionOption a where
  sxFunctionUnsafeSetOption :: SXFunction -> String -> a -> IO ()

instance SXFunctionOption Double where
  sxFunctionUnsafeSetOption (SXFunction rawFun) name' val = do
    name <- newCString name'
    withForeignPtr rawFun (c_sxFunctionSetOptionDouble name (realToFrac val))
    free name
instance SXFunctionOption String where
  sxFunctionUnsafeSetOption (SXFunction rawFun) name' val' = do
    name <- newCString name'
    val <- newCString val'
    withForeignPtr rawFun (c_sxFunctionSetOptionString name val)
    free name
    free val
instance SXFunctionOption Int where
  sxFunctionUnsafeSetOption (SXFunction rawFun) name' val = do
    name <- newCString name'
    withForeignPtr rawFun (c_sxFunctionSetOptionInt name (fromIntegral val))
    free name
instance SXFunctionOption Bool where
  sxFunctionUnsafeSetOption (SXFunction rawFun) name' val = do
    name <- newCString name'
    let intVal = if val then 1 else 0
    withForeignPtr rawFun (c_sxFunctionSetOptionBool name intVal)
    free name

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
