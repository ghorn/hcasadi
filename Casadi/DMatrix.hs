-- DMatrix.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.DMatrix
       (
         DMatrix(..)
       , DMatrixRaw(..)
       , dMatrixTranspose
       , dMatrixToLists
       , dMatrixToList
       , dMatrixFromList
       , dMatrixFromLists
       , dMatrixNewZeros
       , zeros
       , dMatrixSize
       , dMatrixScale
       , dMatrixInv
       ) where

import Casadi.CasadiInterfaceUtils

import Foreign.C
import Foreign.Marshal(newArray)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq

-- the DMatrix data type
data DMatrixRaw = DMatrixRaw
newtype DMatrix = DMatrix (ForeignPtr DMatrixRaw)

instance NFData DMatrix where
  rnf x = x `seq` ()

instance Storable DMatrixRaw where
  sizeOf _ = fromIntegral $ unsafePerformIO c_dMatrixSizeOfAddress
  alignment _ = fromIntegral $ unsafePerformIO c_dMatrixSizeOfAddress

-- foreign imports
foreign import ccall unsafe "dMatrixSizeOfAddress" c_dMatrixSizeOfAddress :: IO CInt
foreign import ccall unsafe "&dMatrixDelete" c_dMatrixDelete :: FunPtr (Ptr DMatrixRaw -> IO ())
foreign import ccall unsafe "dMatrixZeros" c_dMatrixZeros :: CInt -> CInt -> IO (Ptr DMatrixRaw)
foreign import ccall unsafe "dMatrixShow" c_dMatrixShow :: Ptr CChar -> CInt -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixAt" c_dMatrixAt :: (Ptr DMatrixRaw) -> CInt -> CInt -> IO CDouble
foreign import ccall unsafe "dMatrixSetList" c_dMatrixSetList :: CInt -> Ptr CDouble -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixSetLists" c_dMatrixSetLists :: CInt -> CInt -> Ptr CDouble -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixSize1" c_dMatrixSize1 :: (Ptr DMatrixRaw) -> IO CInt
foreign import ccall unsafe "dMatrixSize2" c_dMatrixSize2 :: (Ptr DMatrixRaw) -> IO CInt

foreign import ccall unsafe "dMatrixPlus" c_dMatrixPlus :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixMinus" c_dMatrixMinus :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMM" c_dMM :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixTranspose" c_dMatrixTranspose :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixIsEqual" c_dMatrixIsEqual :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO CInt
foreign import ccall unsafe "dMatrixScale" c_dMatrixScale :: CDouble -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixInv" c_dMatrixInv :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()


----------------- create -------------------------
dMatrixNewZeros :: (Int, Int) -> IO DMatrix
dMatrixNewZeros (n,m) = mask_ $ do
  let n' = safeToCInt n
      m' = safeToCInt m
      
      safeToCInt :: Int -> CInt
      safeToCInt x
        | and [toInteger x <= maxCInt, toInteger x >= minCInt] = fromIntegral x
        | otherwise = error "Error - dMatrixNewZeros dimensions too big"
        where
          maxCInt = fromIntegral (maxBound :: CInt)
          minCInt = fromIntegral (minBound :: CInt)

  mat <- c_dMatrixZeros n' m' >>= newForeignPtr c_dMatrixDelete
  return $ DMatrix mat

zeros :: (Int, Int) -> DMatrix
zeros dimensions = unsafePerformIO $ do
  mOut <- dMatrixNewZeros dimensions
  return mOut

dMatrixFromList :: [Double] -> DMatrix
dMatrixFromList dList = unsafePerformIO $ do
  dListPtr <- newArray (map realToFrac dList)
  DMatrix m0 <- dMatrixNewZeros (length dList, 1)
  withForeignPtr m0 $ c_dMatrixSetList (fromIntegral $ length dList) dListPtr
  return $ DMatrix m0

dMatrixFromLists :: [[Double]] -> DMatrix
dMatrixFromLists dLists = unsafePerformIO $ do
  let rows' = length dLists
      cols' = length (head dLists)
  dListPtr <- newArray $ map realToFrac (concat dLists)
  DMatrix m0 <- dMatrixNewZeros (rows', cols')
  withForeignPtr m0 $ c_dMatrixSetLists (fromIntegral rows') (fromIntegral cols') dListPtr
  return  $ DMatrix m0

---------------- show -------------------
dMatrixShow :: DMatrix -> String
dMatrixShow (DMatrix s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 4096 ' '
  withForeignPtr s $ c_dMatrixShow stringRef (fromIntegral stringLength)
  peekCString stringRef


--------------- getters/setters ---------------------
dMatrixAt :: DMatrix -> (Int,Int) -> IO Double
dMatrixAt (DMatrix matIn) (n,m) = do
  dOut <- withForeignPtr matIn (\matIn' -> c_dMatrixAt matIn' (fromIntegral n) (fromIntegral m))
  return $ realToFrac dOut

---------------- dimensions --------------------
dMatrixSize :: DMatrix -> (Int,Int)
dMatrixSize (DMatrix matIn) = unsafePerformIO $ do
  n <- withForeignPtr matIn c_dMatrixSize1
  m <- withForeignPtr matIn c_dMatrixSize2
  return (fromIntegral n, fromIntegral m)


dMatrixToLists :: DMatrix -> [[Double]]
dMatrixToLists mat = unsafePerformIO $ do
  let f row = mapM (\col -> dMatrixAt mat (row, col)) [0..m-1]
      (n,m) = dMatrixSize mat
  mapM f [0..n-1]

-- turns n by 1 matrix into a list of D, returns error if matrix is not n by 1
dMatrixToList :: DMatrix -> [Double]
dMatrixToList mat = unsafePerformIO $ do
  let (n,m) = dMatrixSize mat
  if m == 1
    then mapM (\row -> dMatrixAt mat (row, 0)) [0..n-1]
    else error "dMatrixToList can only be used on an n by 1 matrix"


------------------------- math ---------------------------------
dMatrixPlus :: DMatrix -> DMatrix -> DMatrix
dMatrixPlus (DMatrix m0) (DMatrix m1) = unsafePerformIO $ do
  let size'
        | sizeM0 == sizeM1 = sizeM0
        | otherwise      = error "dMatrixPlus can't add matrices of different dimensions"
        where
          sizeM0 = dMatrixSize (DMatrix m0)
          sizeM1 = dMatrixSize (DMatrix m1)
  DMatrix mOut <- dMatrixNewZeros size'
  withForeignPtrs3 c_dMatrixPlus m0 m1 mOut
  return $ DMatrix mOut

dMatrixMinus :: DMatrix -> DMatrix -> DMatrix
dMatrixMinus (DMatrix m0) (DMatrix m1) = unsafePerformIO $ do
  let size'
        | sizeM0 == sizeM1 = sizeM0
        | otherwise      = error "dMatrixMinus can't add matrices of different dimensions"
        where
          sizeM0 = dMatrixSize (DMatrix m0)
          sizeM1 = dMatrixSize (DMatrix m1)
  DMatrix mOut <- dMatrixNewZeros size'
  withForeignPtrs3 c_dMatrixMinus m0 m1 mOut
  return $ DMatrix mOut

dMM :: DMatrix -> DMatrix -> DMatrix
dMM (DMatrix m0) (DMatrix m1) = unsafePerformIO $ do
  let size'
        | colsM0 == rowsM1 = (rowsM0, colsM1)
        | otherwise        = error "dMM sees incompatible dimensions"
        where
          (rowsM0, colsM0) = dMatrixSize (DMatrix m0)
          (rowsM1, colsM1) = dMatrixSize (DMatrix m1)

  DMatrix mOut <- dMatrixNewZeros size'
  withForeignPtrs3 c_dMM m0 m1 mOut
  return $ DMatrix mOut

dMatrixTranspose :: DMatrix -> DMatrix
dMatrixTranspose (DMatrix mIn) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros $ dMatrixSize (DMatrix mIn)
  withForeignPtrs2 c_dMatrixTranspose mIn mOut
  return $ DMatrix mOut


dMatrixIsEqual :: DMatrix -> DMatrix -> Bool
dMatrixIsEqual (DMatrix m0) (DMatrix m1) = unsafePerformIO $ do
  isEq <- withForeignPtrs2 c_dMatrixIsEqual m0 m1
  if (isEq == 1)
    then
    return True
    else
    return False

dMatrixScale :: Double -> DMatrix -> DMatrix
dMatrixScale scalar (DMatrix mIn) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros (1,1)
  withForeignPtrs2 (c_dMatrixScale $ realToFrac scalar) mIn mOut
  return $ DMatrix mOut

dMatrixInv :: DMatrix -> DMatrix
dMatrixInv (DMatrix mIn) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros (1,1)
  withForeignPtrs2 c_dMatrixInv mIn mOut
  return $ DMatrix mOut


----------------- typeclass stuff ------------------
instance Show DMatrix where
  show d = dMatrixShow d

instance Eq DMatrix where
  (==) = dMatrixIsEqual
  (/=) d0 d1 = not $ d0 == d1

instance Num DMatrix where
  (+) = dMatrixPlus
  (-) = dMatrixMinus
  (*) m0 m1
    | dMatrixSize m0 == (1,1) = dMatrixScale s0 m1
    | dMatrixSize m1 == (1,1) = dMatrixScale s1 m0
    | otherwise                = dMM m0 m1
      where
        [s0] = dMatrixToList m0
        [s1] = dMatrixToList m1

  abs = error "abs not defined for instance Num DMatrix"
  signum = error "signum not defined for instance Num DMatrix"
  fromInteger i = dMatrixFromList [fromIntegral i]

instance Fractional DMatrix where
  (/) m0 m1 = m0 * (recip m1)
  recip mat = dMatrixInv mat
  fromRational x = dMatrixFromList [fromRational x :: Double]
