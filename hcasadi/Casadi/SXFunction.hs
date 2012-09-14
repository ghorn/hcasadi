{-# OPTIONS_GHC -Wall #-}

module Casadi.SXFunction ( -- * external
                           SXFunction
                         , sxFunctionCreateCallable
                         , sxFunctionCreateCallable'
                           -- * internal
                         , sxFunctionCreate
                         , sxFunctionMakeCallable
                         , sxFunctionMakeCallable'
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
                         , sxFunctionUnsafeEvalMaybes
                         , sxFunctionEvalMaybes
                         ) where

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Control.Exception ( mask_ )
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import Data.Maybe ( fromMaybe )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
import Data.Vector.Storable ( Vector )
import qualified Data.Vector.Storable as V
import Foreign.C ( CDouble(..) )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Marshal ( newArray, mallocArray, free, finalizerFree )

import Casadi.Bindings.SXM ( SXMRaw )
import Casadi.Bindings.SXFunction
import Casadi.WithForeignPtrs ( withForeignPtrs2 )
import Casadi.SXM ( sxmNewEmpty )
import Casadi.SXFunctionOptions ( SXFunctionOption )
import Casadi.SXFunctionOptionsInternal ( sxFunctionUnsafeSetOption )
import Casadi.Types ( SXFunction(..), SXM(..) )

sxFunctionCreateCallable' :: (Functor f, Foldable f, Traversable g)
                             => f SXM -> g SXM
                             -> [SXFunctionOption]
                             -> IO (f (Vector CDouble) -> IO (g (Vector CDouble)))
sxFunctionCreateCallable' inputs outputs options = do
  fun <- sxFunctionCreate (fmap Just inputs) (fmap Just outputs) options
  sxFunctionMakeCallable' fun inputs outputs

sxFunctionCreateCallable :: (Foldable f, Traversable g)
                            => f (Maybe SXM) -> g (Maybe SXM)
                            -> [SXFunctionOption]
                            -> IO (f (Maybe (Vector CDouble)) -> IO (g (Maybe (Vector CDouble))))
sxFunctionCreateCallable inputs outputs options = do
  fun <- sxFunctionCreate inputs outputs options
  sxFunctionMakeCallable fun inputs outputs

sxFunctionCreate :: (Foldable f, Traversable g)
                    => f (Maybe SXM) -> g (Maybe SXM)
                    -> [SXFunctionOption]
                    -> IO SXFunction
sxFunctionCreate inputs outputs options = do
  empty <- sxmNewEmpty
  let inputList  = map (fromMaybe empty) $ F.toList inputs
      outputList = map (fromMaybe empty) $ F.toList outputs
  sxFunctionCreateFromLists inputList outputList options

sxFunctionMakeCallable' :: (Functor f, Foldable f, Traversable g)
                           => SXFunction -> f SXM -> g SXM
                           -> IO (f (Vector CDouble) -> IO (g (Vector CDouble)))
sxFunctionMakeCallable' fun inputs outputs = do
  f <- sxFunctionMakeCallable fun (fmap Just inputs) (fmap Just outputs)
  return $ \userInputs -> do
    outs <- f (fmap Just userInputs)
    let err = error "sxFunctionCreateCallable': should get only Just, no Nothing..."
    return $ fmap (fromMaybe err) outs

sxFunctionMakeCallable :: (Foldable f, Traversable g) =>
                          SXFunction -> f (Maybe SXM) -> g (Maybe SXM)
                          -> IO (f (Maybe (Vector CDouble)) -> IO (g (Maybe (Vector CDouble))))
sxFunctionMakeCallable fun inputs outputs = do
  let inputList = F.toList inputs
      outputList = F.toList outputs
  
  outputSizes <- mapM (sxFunctionOutputSize fun) [0..(length outputList - 1)]
      
  return $ \userInputs -> do
    let -- make sure userInputs has Nothing/Just in the same places as the original SXM inputs
        -- and convert the userInputs to a list of [Maybe (ForeignPtr CDouble, Int)]
        inputForeignPtrsLen :: [Maybe (ForeignPtr CDouble, Int)]
        inputForeignPtrsLen = zipWith f inputList (F.toList userInputs)
          where
            f :: Maybe SXM -> Maybe (Vector CDouble) ->  Maybe (ForeignPtr CDouble, Int)
            f (Just _) (Just v) = Just $ V.unsafeToForeignPtr0 v
            f Nothing Nothing = Nothing
            f (Just _) Nothing = error "you passed Nothing into a sxFunction which you created with Just"
            f Nothing (Just _) = error "you passed Just into a sxFunction which you created with Nothing"
            
        inputPtrsLen :: [Maybe (Ptr CDouble, Int)]
        inputPtrsLen = map (fmap (first unsafeForeignPtrToPtr)) inputForeignPtrsLen
    outputVecs <- map (uncurry V.unsafeFromForeignPtr0 <$>) <$> sxFunctionEvalMaybes fun outputSizes inputPtrsLen
    mapM_ (F.mapM_ (touchForeignPtr . fst)) inputForeignPtrsLen

    let -- make sure userOutputs has Nothing/Just in the same places as original SXM inputs
        f (Just x:xs) (Just _) = (xs, Just x)
        f (Nothing:xs) Nothing = (xs, Nothing)
        f (Nothing:_) (Just _) = error "sxFunctionCreate returned Nothing with Just SXM input"
        f (Just _:_) Nothing = error "sxFunctionCreate returned Just with Nothing SXM input"
        f [] _ = error "after evaluating sxFunction, got too few inputs"
    return $ case T.mapAccumL f outputVecs outputs of
      ([], ret) -> ret
      _ -> error "after evaluating sxFunction, got too many outputs"

-- | Create SXFunction from list of inputs and ouputs.
sxFunctionCreateFromLists :: [SXM] -> [SXM] -> [SXFunctionOption] -> IO SXFunction
sxFunctionCreateFromLists inputs outputs options = mask_ $ do
  -- turn input/output SXM lists into [Ptr SXMRaw]
  let unsafeInputPtrs :: [Ptr SXMRaw]
      unsafeInputPtrs = map (\(SXM p) -> unsafeForeignPtrToPtr p) inputs
      
      unsafeOutputPtrs :: [Ptr SXMRaw]
      unsafeOutputPtrs = map (\(SXM p) -> unsafeForeignPtrToPtr p) outputs
      
      nIn  = fromIntegral $ length inputs
      nOut = fromIntegral $ length outputs

  -- turn [Ptr SXMRaw] into Ptr (Ptr SXMRaw)
  inputPtrArray <- newArray unsafeInputPtrs
  outputPtrArray <- newArray unsafeOutputPtrs
  
  -- create SXFunction
  funRaw <- c_sxFunctionCreate
            inputPtrArray  nIn
            outputPtrArray nOut  >>= newForeignPtr c_sxFunctionDelete

  -- free arrays
  free inputPtrArray
  free outputPtrArray

  -- touch all [ForeignPtr SXMRaw] for unsafeForeignPtrToPtr safety
  mapM_ (\(SXM p) -> touchForeignPtr p) inputs
  mapM_ (\(SXM p) -> touchForeignPtr p) outputs

  let fun = SXFunction funRaw
  mapM_ (sxFunctionUnsafeSetOption fun) options
  withForeignPtr funRaw c_sxFunctionInit
  
  return fun

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
-- | provide pointers and the length of their respective arrays
--   to input data (which won't be changed) and output data (which will be changed)
sxFunctionUnsafeEvalMaybes :: SXFunction -> [Maybe (Ptr CDouble, Int)] -> [Maybe (Ptr CDouble, Int)] -> IO (Maybe Int)
sxFunctionUnsafeEvalMaybes (SXFunction rawFun) inputs outputs = do
  let (inputPtrs,inputSizes) = unzip $ map (fromMaybe (nullPtr,0)) inputs
      numInputs = length inputs

      (outputPtrs,outputSizes) = unzip $ map (fromMaybe (nullPtr,0)) outputs
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

sxFunctionEvalMaybes :: SXFunction -> [Int] -> [Maybe (Ptr CDouble, Int)] -> IO [Maybe (ForeignPtr CDouble, Int)]
sxFunctionEvalMaybes fun outputSizes inputs = do
  let f :: Int -> IO (Maybe (Ptr CDouble, Int))
      f 0 = return Nothing
      f k = (\p -> Just (p,k)) <$> mallocArray k
  outputPtrs <- mapM f outputSizes
  ret <- sxFunctionUnsafeEvalMaybes fun inputs outputPtrs

  let g :: Maybe (Ptr CDouble, Int) -> IO (Maybe (ForeignPtr CDouble, Int))
      g (Just (p,k)) = do
        fp <- newForeignPtr finalizerFree p
        return (Just (fp,k))
      g Nothing = return Nothing
  outputs <- mapM g outputPtrs
  return $ case ret of
    Nothing -> outputs
    Just n -> error $ "sxFunctionUnsafeEvalMaybes returned error code " ++ show n
