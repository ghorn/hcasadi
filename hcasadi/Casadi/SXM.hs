{-# OPTIONS_GHC -Wall #-}

module Casadi.SXM( SXM
                 , sxmNewDouble
                 , sxmNewIntegral
                 , sxmShow
                 , sxmSym
                 , sxmSymVec
                 , sxmSymMat
                 , sxmSize
                 , sxmAt
                 , sxmVecCat
                 , sxmVertCat
                 , sxmHorzCat
                 , sxmToLists
                 , sxmToList
                 ) where

import Foreign.C ( CInt(..), newCString )
import Control.Exception ( mask_ )
import Foreign.ForeignPtr ( newForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal ( newArray )
import Foreign.Ptr ( Ptr )

import Casadi.Bindings.SXM
import Casadi.Types ( SXM(..), sxmShow )

------------------- create symbolic -------------------------------
sxmSym :: String -> IO SXM
sxmSym = sxmSymMat (1,1)

sxmSymVec :: Int -> String -> IO SXM
sxmSymVec n = sxmSymMat (n,1)

sxmSymMat :: (Int,Int) -> String -> IO SXM
sxmSymMat (n,m) name = mask_ $ do
  cName <- newCString name
  sym' <- c_sxmCreateSymbolic (fromIntegral n) (fromIntegral m) cName >>= newForeignPtr c_sxmDelete
  return $ SXM sym'

---------------------- create numeric -----------------------
sxmNewDouble :: Double -> IO SXM
sxmNewDouble val = mask_ $ do
    f <- c_sxmNewDouble (realToFrac val) >>= newForeignPtr c_sxmDelete
    return $ SXM f

sxmNewInt :: Int -> IO SXM
sxmNewInt val = mask_ $ do
    f <- c_sxmNewInt (fromIntegral val) >>= newForeignPtr c_sxmDelete
    return $ SXM f

sxmNewIntegral :: Integral a => a -> IO SXM
sxmNewIntegral val
  | withinCIntBounds val = sxmNewInt (fromIntegral val)
  | otherwise            = error "input out of range of CInt in sxmNewIntegral"
  where
    withinCIntBounds x = and [fromIntegral x <= maxCInt, fromIntegral x >= minCInt]
    maxCInt = toInteger (maxBound :: CInt)
    minCInt = toInteger (minBound :: CInt)

--------- size -------
-- | get (rows,cols)
sxmSize :: SXM -> IO (Int,Int)
sxmSize (SXM sxm) = mask_ $ do
  s1 <- withForeignPtr sxm c_sxmSize1
  s2 <- withForeignPtr sxm c_sxmSize2
  return (fromIntegral s1, fromIntegral s2)

-- | look up scalar
sxmAt :: SXM -> (Int,Int) -> IO SXM
sxmAt (SXM sxm) (row,col) = mask_ $ do
  s1 <- withForeignPtr sxm (\s -> c_sxmAt s (fromIntegral row) (fromIntegral col)) >>= newForeignPtr c_sxmDelete
  return (SXM s1)

------------ to/from lists ------------
wrapSXMList :: (Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXMRaw)) -> [SXM] -> IO SXM
wrapSXMList c_fun inputs = mask_ $ do
  -- turn input SXMatrix lists into [Ptr SXMRaw]
  let unsafeInputPtrs :: [Ptr SXMRaw]
      unsafeInputPtrs = map (\(SXM mat) -> unsafeForeignPtrToPtr mat) inputs
      
      nIn  = fromIntegral $ length inputs
  
  -- turn [Ptr SXMRaw] into Ptr (Ptr SXMRaw)
  inputPtrArray <- newArray unsafeInputPtrs
  
  -- create SXFunction
  out <- c_fun inputPtrArray nIn >>= newForeignPtr c_sxmDelete
  
  -- touch all [ForeignPtr SXMRaw] for unsafeForeignPtrToPtr safety
  mapM_ (\(SXM d) -> touchForeignPtr d) inputs
  return (SXM out)

-- from lists
sxmVecCat, sxmVertCat, sxmHorzCat :: [SXM] -> IO SXM
sxmVecCat  = wrapSXMList c_sxmVecCat
sxmVertCat = wrapSXMList c_sxmVertCat
sxmHorzCat = wrapSXMList c_sxmHorzCat

-- to lists
sxmToLists :: SXM -> IO [[SXM]]
sxmToLists mat = do
  (rows,cols) <- sxmSize mat
  mapM sequence [[sxmAt mat (r,c) | c <- [0..cols-1]] | r <- [0..rows-1]]

sxmToList :: SXM -> IO [SXM]
sxmToList vec = do
  rowsCols <- sxmSize vec
  case rowsCols of
    (n,1) -> sequence [sxmAt vec (r,0) | r <- [0..n-1]]
    (1,n) -> sequence [sxmAt vec (0,c) | c <- [0..n-1]]
    _ -> error $ "sxmToList got matrix with dimensions: " ++ show rowsCols
