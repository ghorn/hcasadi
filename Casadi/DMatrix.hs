-- DMatrix.hs

{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-cse -fno-full-laziness #-}
{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}

module Casadi.DMatrix
       (
         DMatrix(..)
       , DMatrixRaw(..)
       , dMatrixNewZeros
       ) where

import Casadi.CasadiInterfaceUtils
import Casadi.Matrix
import Casadi.SXFunctionRaw

import Foreign.C
import Foreign.Marshal
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Control.Exception(mask_)
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq
import Data.List(intersperse)
import Data.Tuple(swap)


-- the DMatrix data type
data DMatrixRaw = DMatrixRaw
newtype DMatrix = DMatrix (ForeignPtr DMatrixRaw)

instance NFData DMatrix where
  rnf x = x `seq` ()

-- foreign imports
foreign import ccall unsafe "&dMatrixDelete" c_dMatrixDelete
  :: FunPtr (Ptr DMatrixRaw -> IO ())
foreign import ccall unsafe "dMatrixZeros" c_dMatrixZeros
  :: CInt -> CInt -> IO (Ptr DMatrixRaw)
foreign import ccall unsafe "dMatrixAt" c_dMatrixAt
  :: (Ptr DMatrixRaw) -> CInt -> CInt -> IO CDouble
foreign import ccall unsafe "dMatrixSetToList" c_dMatrixSetToList
  :: CInt -> Ptr CDouble -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixSetFromList" c_dMatrixSetFromList
  :: CInt -> Ptr CDouble -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixSetFromLists" c_dMatrixSetFromLists
  :: CInt -> CInt -> Ptr CDouble -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixSize1" c_dMatrixSize1
  :: (Ptr DMatrixRaw) -> IO CInt
foreign import ccall unsafe "dMatrixSize2" c_dMatrixSize2
  :: (Ptr DMatrixRaw) -> IO CInt
foreign import ccall unsafe "dMatrixPlus" c_dMatrixPlus
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixMinus" c_dMatrixMinus
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixNegate" c_dMatrixNegate
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMM" c_dMM
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixTranspose" c_dMatrixTranspose
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixIsEqual" c_dMatrixIsEqual
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO CInt
foreign import ccall unsafe "dMatrixScale" c_dMatrixScale
  :: CDouble -> (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixInv" c_dMatrixInv
  :: (Ptr DMatrixRaw) -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "dMatrixVertcat" c_dMatrixVertcat
  :: Ptr (Ptr DMatrixRaw) -> CInt -> (Ptr DMatrixRaw) -> IO ()
foreign import ccall unsafe "sxFunctionEvaluateDMatrix" c_sxFunctionEvaluateDMatrix
  :: CInt -> Ptr (Ptr DMatrixRaw) -> CInt -> Ptr (Ptr DMatrixRaw) -> Ptr SXFunctionRaw -> IO ()


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


dMatrixZeros :: (Int, Int) -> DMatrix
{-# NOINLINE dMatrixZeros #-}
dMatrixZeros dimensions = unsafePerformIO $ do
  mOut <- dMatrixNewZeros dimensions
  return mOut


dMatrixToList :: DMatrix -> [Double]
{-# NOINLINE dMatrixToList #-}
dMatrixToList (DMatrix dMatRaw) = unsafePerformIO $ do
  let (n,m) = size (DMatrix dMatRaw)
  if (m /= 1)
    then error "dMatrixToList can only be used on an n by 1 matrix"
    else do dListPtr <- mallocArray n
            withForeignPtr dMatRaw $ c_dMatrixSetToList (fromIntegral n) dListPtr
            listOut <- peekArray n dListPtr

            return $ map realToFrac listOut


dMatrixFromList :: [Double] -> DMatrix
{-# NOINLINE dMatrixFromList #-}
dMatrixFromList dList = unsafePerformIO $ do
  dListPtr <- newArray (map realToFrac dList)
  DMatrix m0 <- dMatrixNewZeros (length dList, 1)
  withForeignPtr m0 $ c_dMatrixSetFromList (fromIntegral $ length dList) dListPtr
  return $ DMatrix m0


dMatrixFromLists :: [[Double]] -> DMatrix
{-# NOINLINE dMatrixFromLists #-}
dMatrixFromLists dLists = unsafePerformIO $ do
  let rows' = length dLists
      cols' = length (head dLists)
  dListPtr <- newArray $ map realToFrac (concat dLists)
  DMatrix m0 <- dMatrixNewZeros (rows', cols')
  withForeignPtr m0 $ c_dMatrixSetFromLists (fromIntegral rows') (fromIntegral cols') dListPtr
  return  $ DMatrix m0


--------------- getters/setters ---------------------
dMatrixAt :: DMatrix -> (Int,Int) -> IO Double
dMatrixAt (DMatrix matIn) (n,m) = do
  dOut <- withForeignPtr matIn (\matIn' -> c_dMatrixAt matIn' (fromIntegral n) (fromIntegral m))
  return $ realToFrac dOut


---------------- dimensions --------------------
dMatrixSize :: DMatrix -> (Int,Int)
{-# NOINLINE dMatrixSize #-}
dMatrixSize (DMatrix matIn) = unsafePerformIO $ do
  n <- withForeignPtr matIn c_dMatrixSize1
  m <- withForeignPtr matIn c_dMatrixSize2
  return (fromIntegral n, fromIntegral m)


dMatrixToLists :: DMatrix -> [[Double]]
{-# NOINLINE dMatrixToLists #-}
dMatrixToLists mat = unsafePerformIO $ do
  let f row = mapM (\col -> dMatrixAt mat (row, col)) [0..m-1]
      (n,m) = dMatrixSize mat
  mapM f [0..n-1]


------------------------- math ---------------------------------
dMatrixPlus :: DMatrix -> DMatrix -> DMatrix
{-# NOINLINE dMatrixPlus #-}
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
{-# NOINLINE dMatrixMinus #-}
dMatrixMinus (DMatrix m0) (DMatrix m1) = unsafePerformIO $ do
  let size'
        | sizeM0 == sizeM1 = sizeM0
        | otherwise        = error $ "dMatrixMinus can't subtract " ++ (show (dMatrixSize (DMatrix m0))) ++ " matrix\n" ++ (show (DMatrix m0)) ++ "\nby " ++ (show (dMatrixSize (DMatrix m1))) ++ " matrix\n" ++ (show (DMatrix m1))
        where
          sizeM0 = dMatrixSize (DMatrix m0)
          sizeM1 = dMatrixSize (DMatrix m1)
  DMatrix mOut <- dMatrixNewZeros size'
  withForeignPtrs3 c_dMatrixMinus m0 m1 mOut
  return $ DMatrix mOut


dMatrixNegate :: DMatrix -> DMatrix
{-# NOINLINE dMatrixNegate #-}
dMatrixNegate (DMatrix m0) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros (dMatrixSize (DMatrix m0))
  withForeignPtrs2 c_dMatrixNegate m0 mOut
  return $ DMatrix mOut


dMM :: DMatrix -> DMatrix -> DMatrix
{-# NOINLINE dMM #-}
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
{-# NOINLINE dMatrixTranspose #-}
dMatrixTranspose (DMatrix mIn) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros $ swap (dMatrixSize (DMatrix mIn))
  withForeignPtrs2 c_dMatrixTranspose mIn mOut
  return $ DMatrix mOut


dMatrixIsEqual :: DMatrix -> DMatrix -> Bool
{-# NOINLINE dMatrixIsEqual #-}
dMatrixIsEqual (DMatrix m0) (DMatrix m1) = unsafePerformIO $ do
  isEq <- withForeignPtrs2 c_dMatrixIsEqual m0 m1
  if (isEq == 1)
    then
    return True
    else
    return False


dMatrixScale :: Double -> DMatrix -> DMatrix
{-# NOINLINE dMatrixScale #-}
dMatrixScale scalar (DMatrix mIn) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros (1,1)
  withForeignPtrs2 (c_dMatrixScale $ realToFrac scalar) mIn mOut
  return $ DMatrix mOut


dMatrixInv :: DMatrix -> DMatrix
{-# NOINLINE dMatrixInv #-}
dMatrixInv (DMatrix mIn) = unsafePerformIO $ do
  DMatrix mOut <- dMatrixNewZeros (1,1)
  withForeignPtrs2 c_dMatrixInv mIn mOut
  return $ DMatrix mOut


dMatrixVertcat :: [DMatrix] -> DMatrix
{-# NOINLINE dMatrixVertcat #-}
dMatrixVertcat inputs = unsafePerformIO $ do
  -- turn input DMatrix lists into [Ptr DMatrixRaw]
  let unsafeInputPtrs :: [Ptr DMatrixRaw]
      unsafeInputPtrs = map (\(DMatrix mat) -> unsafeForeignPtrToPtr mat) inputs
      nIn  = fromIntegral $ length inputs
      
  -- turn [Ptr SXMatrixRaw] into Ptr (Ptr DMatrixRaw)
  inputPtrArray <- newArray unsafeInputPtrs

  DMatrix mOutRaw <- dMatrixNewZeros (sum $ map rows inputs, cols (head inputs))

  withForeignPtr mOutRaw $ c_dMatrixVertcat inputPtrArray nIn

  -- touch all [ForeignPtr DMatrixRaw] for unsafeForeignPtrToPtr safety
  mapM_ (\(DMatrix d) -> touchForeignPtr d) inputs

  return (DMatrix mOutRaw)


----------------- typeclass stuff ------------------
instance Show DMatrix where
  show d = f (dMatrixSize d)
    where
      f (1,1) = show $ toLists d
      f (_,1) = show $ toList d
      f (1,_) = show $ toLists d
      f (_,_) = '[': (concat $ intersperse "\n " $ map show (toLists d)) ++ "]"

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

  negate = dMatrixNegate


instance Fractional DMatrix where
  (/) m0 m1 = m0 * (recip m1)
  recip mat = dMatrixInv mat
  fromRational x = dMatrixFromList [fromRational x :: Double]


instance Matrix DMatrix Double DMatrixRaw where
  trans = dMatrixTranspose
  size = dMatrixSize
  rows = fst . dMatrixSize
  cols = snd . dMatrixSize
  toList = dMatrixToList
  toLists = dMatrixToLists
  fromList = dMatrixFromList
  fromLists = dMatrixFromLists
  vertcat = dMatrixVertcat
  inv = dMatrixInv
  scale = dMatrixScale
  zeros = dMatrixZeros

  c_sxFunctionEvaluate _ = c_sxFunctionEvaluateDMatrix
  getForeignPtr (DMatrix r) = r
  newZeros = dMatrixNewZeros

instance Boundable DMatrix where
  bound xs (lbs, ubs) = fromList $ zipWith boundDouble (toList xs) (zip (toList lbs) (toList ubs))
    where
      boundDouble x (lb, ub)
        | ub < lb = error $ "in boundDouble, ub (" ++ show ub ++ ") < lb (" ++ show lb ++ ")"
        | x < lb = lb
        | x > ub = ub
        | otherwise = x
