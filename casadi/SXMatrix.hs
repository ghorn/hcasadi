-- SXMatrix.hs

--{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SXMatrix(SXMatrix(..), SXMatrixRaw, sxMatrixTranspose, sxMatrixCreateSymbolic, sxMatrixToLists, sxMatrixToList) where

import SX
import CasadiInterfaceUtils

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)

-- the SXMatrix/SXVector data type
data SXMatrixRaw = SXMatrixRaw
newtype SXMatrix = SXMatrix (ForeignPtr SXMatrixRaw)
newtype SXVector = SXVector (ForeignPtr SXMatrixRaw)

-- foreign imports
foreign import ccall "sxMatrixCreateSymbolic" c_sxMatrixCreateSymbolic :: Ptr CChar -> CInt -> CInt -> IO (Ptr SXMatrixRaw)
foreign import ccall "sxMatrixCopy" c_sxMatrixCopy :: (Ptr SXMatrixRaw) -> IO (Ptr SXMatrixRaw)
foreign import ccall "&sxMatrixDelete" c_sxMatrixDelete :: FunPtr (Ptr SXMatrixRaw -> IO ())
foreign import ccall "sxMatrixZeros" c_sxMatrixZeros :: CInt -> CInt -> IO (Ptr SXMatrixRaw)
foreign import ccall "sxMatrixShow" c_sxMatrixShow :: Ptr CChar -> CInt -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMatrixAt" c_sxMatrixAt :: (Ptr SXMatrixRaw) -> CInt -> CInt -> (Ptr SXRaw) -> IO ()
foreign import ccall "sxMatrixSet" c_sxMatrixSet :: (Ptr SXRaw) -> CInt -> CInt -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMatrixSize1" c_sxMatrixSize1 :: (Ptr SXMatrixRaw) -> IO CInt
foreign import ccall "sxMatrixSize2" c_sxMatrixSize2 :: (Ptr SXMatrixRaw) -> IO CInt

foreign import ccall "sxMatrixPlus" c_sxMatrixPlus :: (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMatrixMinus" c_sxMatrixMinus :: (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMM" c_sxMM :: (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMatrixTranspose" c_sxMatrixTranspose :: (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO ()
foreign import ccall "sxMatrixIsEqual" c_sxMatrixIsEqual :: (Ptr SXMatrixRaw) -> (Ptr SXMatrixRaw) -> IO CInt

----------------- create -------------------------
sxMatrixCreateSymbolic :: String -> (Integer, Integer) -> SXMatrix
sxMatrixCreateSymbolic prefix (n,m) = unsafePerformIO $ mask_ $ do
  cPrefix <- newCString prefix
  mat <- c_sxMatrixCreateSymbolic cPrefix (fromIntegral n) (fromIntegral m) >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix mat

sxMatrixCopy :: SXMatrix -> SXMatrix
sxMatrixCopy (SXMatrix old) = unsafePerformIO $ mask_ $ do
  new <- withForeignPtr old c_sxMatrixCopy >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix new

sxVectorCreateSymbolic :: String -> Integer -> SXVector
sxVectorCreateSymbolic prefix n = (SXVector p)
  where
    (SXMatrix p) = sxMatrixCreateSymbolic prefix (n,1)

sxMatrixZeros :: Integral a => (a, a) -> SXMatrix
sxMatrixZeros (n,m) = unsafePerformIO $ mask_ $ do
  let n' = safeToCInt n
      m' = safeToCInt m
      
      safeToCInt :: (Integral a) => a -> CInt
      safeToCInt x
        | and [toInteger x <= maxCInt, toInteger x >= minCInt] = fromIntegral x
        | otherwise = error "Error - sxMatrixZeros dimensions too big"
        where
          maxCInt = fromIntegral (maxBound :: CInt)
          minCInt = fromIntegral (minBound :: CInt)

  mat <- c_sxMatrixZeros n' m' >>= newForeignPtr c_sxMatrixDelete
  return $ SXMatrix mat


---------------- show -------------------
sxMatrixShow :: SXMatrix -> String
sxMatrixShow (SXMatrix s) = unsafePerformIO $ do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxMatrixShow stringRef (fromIntegral stringLength)
  peekCString stringRef

sxVectorShow :: SXVector -> String
sxVectorShow (SXVector s) = sxMatrixShow (SXMatrix s)


--------------- getters/setters ---------------------
sxMatrixAt :: SXMatrix -> (Integer,Integer) -> SX
sxMatrixAt (SXMatrix matIn) (n,m) = unsafePerformIO $ do
  let SX sxOut = sxNewInteger 0
  withForeignPtrs2 (\matIn' sxOut' -> c_sxMatrixAt matIn' (fromIntegral n) (fromIntegral m) sxOut') matIn sxOut
  return (SX sxOut)

sxMatrixSet :: SXMatrix -> (Integer,Integer) -> SX -> SXMatrix
sxMatrixSet (SXMatrix matIn) (n,m) (SX val) = unsafePerformIO $ do
  let SXMatrix matOut = sxMatrixCopy (SXMatrix matIn)
      n' = fromIntegral n
      m' = fromIntegral m
  withForeignPtrs2 (\val' matOut' -> c_sxMatrixSet val' n' m' matOut') val matOut
  return (SXMatrix matOut)

sxVectorAt :: SXVector -> Integer -> SX
sxVectorAt (SXVector vecIn) n = sxMatrixAt (SXMatrix vecIn) (n,0)


---------------- dimensions --------------------
sxMatrixSize :: SXMatrix -> (Integer,Integer)
sxMatrixSize (SXMatrix matIn) = unsafePerformIO $ do
  n <- withForeignPtr matIn c_sxMatrixSize1
  m <- withForeignPtr matIn c_sxMatrixSize2
  return (fromIntegral n, fromIntegral m)

sxVectorLength :: SXVector -> Integer
sxVectorLength (SXVector vecIn)
  | m == 1    = n
  | otherwise = error "Error - sxVectorLength got size: (n, m /= 1)"
  where (n,m) = sxMatrixSize (SXMatrix vecIn)

sxMatrixToLists :: SXMatrix -> [[SX]]
sxMatrixToLists mat = [[sxMatrixAt mat (row, col) | col <- [0..m-1]] | row <- [0..n-1]]
  where
    (n,m) = sxMatrixSize mat

-- turns n by 1 matrix into a list of SX, returns error if matrix is not n by 1
sxMatrixToList :: SXMatrix -> [SX]
sxMatrixToList mat
  | m == 1    = [sxMatrixAt mat (row, 0) | row <- [0..n-1]]
  | otherwise = error "sxMatrixToList can only be used on an n by 1 matrix"
  where
    (n,m) = sxMatrixSize mat

------------------------- math ---------------------------------
sxMatrixPlus :: SXMatrix -> SXMatrix -> SXMatrix
sxMatrixPlus (SXMatrix m0) (SXMatrix m1) = unsafePerformIO $ do
  let size'
        | sizeM0 == sizeM1 = sizeM0
        | otherwise      = error "sxMatrixPlus can't add matrices of different dimensions"
        where
          sizeM0 = sxMatrixSize (SXMatrix m0)
          sizeM1 = sxMatrixSize (SXMatrix m1)
  let SXMatrix mOut = sxMatrixZeros size'
  withForeignPtrs3 c_sxMatrixPlus m0 m1 mOut
  return $ SXMatrix mOut

sxMatrixMinus :: SXMatrix -> SXMatrix -> SXMatrix
sxMatrixMinus (SXMatrix m0) (SXMatrix m1) = unsafePerformIO $ do
  let size'
        | sizeM0 == sizeM1 = sizeM0
        | otherwise      = error "sxMatrixMinus can't add matrices of different dimensions"
        where
          sizeM0 = sxMatrixSize (SXMatrix m0)
          sizeM1 = sxMatrixSize (SXMatrix m1)
  let SXMatrix mOut = sxMatrixZeros size'
  withForeignPtrs3 c_sxMatrixMinus m0 m1 mOut
  return $ SXMatrix mOut

sxVectorPlus :: SXVector -> SXVector -> SXVector
sxVectorPlus (SXVector v0) (SXVector v1) = (SXVector vOut)
  where
    (SXMatrix vOut) = sxMatrixPlus (SXMatrix v0) (SXMatrix v1)

sxVectorMinus :: SXVector -> SXVector -> SXVector
sxVectorMinus (SXVector v0) (SXVector v1) = (SXVector vOut)
  where
    (SXMatrix vOut) = sxMatrixMinus (SXMatrix v0) (SXMatrix v1)

sxMM :: SXMatrix -> SXMatrix -> SXMatrix
sxMM (SXMatrix m0) (SXMatrix m1) = unsafePerformIO $ do
  let size'
        | colsM0 == rowsM1 = (rowsM0, colsM1)
        | otherwise        = error "sxMM sees incompatible dimensions"
        where
          (rowsM0, colsM0) = sxMatrixSize (SXMatrix m0)
          (rowsM1, colsM1) = sxMatrixSize (SXMatrix m1)

  let SXMatrix mOut = sxMatrixZeros size'
  withForeignPtrs3 c_sxMM m0 m1 mOut
  return $ SXMatrix mOut

sxMV :: SXMatrix -> SXVector -> SXVector
sxMV (SXMatrix mat) (SXVector vIn) = SXVector vOut
  where
    SXMatrix vOut = sxMM (SXMatrix mat) (SXMatrix vIn)

sxVM :: SXVector -> SXMatrix -> SXVector
sxVM (SXVector vIn) (SXMatrix mat) = SXVector vOut
  where
    SXMatrix vOut = sxMM (sxMatrixTranspose (SXMatrix vIn)) (SXMatrix mat)

sxMatrixTranspose :: SXMatrix -> SXMatrix
sxMatrixTranspose (SXMatrix mIn) = unsafePerformIO $ do
  let SXMatrix mOut = sxMatrixZeros (m,n)
        where
          (n,m) = sxMatrixSize (SXMatrix mIn)
  withForeignPtrs2 c_sxMatrixTranspose mIn mOut
  return $ SXMatrix mOut


sxMatrixIsEqual :: SXMatrix -> SXMatrix -> Bool
sxMatrixIsEqual (SXMatrix m0) (SXMatrix m1) = unsafePerformIO $ do
  isEq <- withForeignPtrs2 c_sxMatrixIsEqual m0 m1
  if (isEq == 1)
    then
    return True
    else
    return False

sxVectorIsEqual :: SXVector -> SXVector -> Bool
sxVectorIsEqual (SXVector v0) (SXVector v1) = unsafePerformIO $ do
  isEq <- withForeignPtrs2 c_sxMatrixIsEqual v0 v1
  if (isEq == 1)
    then
    return True
    else
    return False


----------------- typeclass stuff ------------------
instance Show SXMatrix where
  show sx = sxMatrixShow sx

instance Show SXVector where
  show sx = sxVectorShow sx

instance Eq SXMatrix where
  (==) = sxMatrixIsEqual
  (/=) sx0 sx1 = not $ sx0 == sx1

instance Eq SXVector where
  (==) = sxVectorIsEqual
  (/=) sx0 sx1 = not $ sx0 == sx1

instance Num SXMatrix where
  (+) = sxMatrixPlus
  (-) = sxMatrixMinus
  (*) = sxMM
  abs = error "abs not defined for instance Num SXMatrix"
  signum = error "signum not defined for instance Num SXMatrix"
  fromInteger = error "fromInteger not defined for instance Num SXMatrix"

instance Num SXVector where
  (+) = sxVectorPlus
  (-) = sxVectorMinus
  (*) = error "(*) not defined for instance Num SXVector"
  abs = error "abs not defined for instance Num SXVector"
  signum = error "signum not defined for instance Num SXVector"
  fromInteger = error "fromInteger not defined for instance Num SXVector"

main :: IO ()
main = do 
  let mA = sxMatrixCreateSymbolic "A" (2,2)
      mB = sxMatrixCreateSymbolic "B" (2,2)
      mC = sxMatrixCreateSymbolic "C" (4,1)
      u = sxVectorCreateSymbolic "u" 3
      v = sxVectorCreateSymbolic "u" 3
  
  print $ mA == mB
  print $ mA == mA
  print $ mA * mB
  print $ mA + mB
  print $ u + v
  print $ u - v

  print $ sxMatrixToLists mA
  print $ sxMatrixToList mC
