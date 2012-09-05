{-# OPTIONS_GHC -Wall #-}

module Casadi.SXM( SXM(..)
                 , sxmNewDouble
                 , sxmNewIntegral
                 , sxmShow
                 , sym
                 , symVec
                 , symMat
                 , sxmSize
                 , sxmAt
                 , sxmVertCat
                 , sxmHorzCat
                   -- * internal
                 , SXMRaw
                 , c_sxmDelete
                 ) where

import Foreign.C ( CInt(..), newCString, newCStringLen, peekCString )
import Control.Exception ( mask_ )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal ( newArray )
import Foreign.Ptr ( Ptr )

import Casadi.Bindings.SXM

-- the SX data type
newtype SXM = SXM (ForeignPtr SXMRaw)
--instance Show SXM where
--  {-# NOINLINE show #-}
--  show s = unsafePerformIO $ sxShow s


------------------- create symbolic -------------------------------
sym :: String -> IO SXM
sym = symMat (1,1)

symVec :: Int -> String -> IO SXM
symVec n = symMat (n,1)

symMat :: (Int,Int) -> String -> IO SXM
symMat (n,m) name = mask_ $ do
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

--------- show --------
sxmShow :: SXM -> IO String
sxmShow (SXM s) = do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxmShow stringRef (fromIntegral stringLength)
  peekCString stringRef

--------- size -------
-- | get (rows,cols)
sxmSize :: SXM -> IO (Int,Int)
sxmSize (SXM sxm) = mask_ $ do
  s1 <- withForeignPtr sxm c_sxmSize1
  s2 <- withForeignPtr sxm c_sxmSize2
  return (fromIntegral s1, fromIntegral s2)

-- | look up scalar
sxmAt :: SXM -> IO SXM
sxmAt (SXM sxm) = mask_ $ do
  s1 <- withForeignPtr sxm c_sxmAt >>= newForeignPtr c_sxmDelete
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

sxmVertCat, sxmHorzCat :: [SXM] -> IO SXM
sxmVertCat = wrapSXMList c_sxmVertCat
sxmHorzCat = wrapSXMList c_sxmHorzCat
