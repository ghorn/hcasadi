{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.Bindings.SXM( SXM(..)
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

import Foreign.C ( CChar(..), CDouble(..), CInt(..), newCString, newCStringLen, peekCString )
import Control.Exception ( mask_ )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )
import Foreign.Marshal ( newArray )
import Foreign.Ptr ( FunPtr, Ptr )

-- the SX data type
data SXMRaw
newtype SXM = SXM (ForeignPtr SXMRaw)
--instance Show SXM where
--  {-# NOINLINE show #-}
--  show s = unsafePerformIO $ sxShow s


foreign import ccall unsafe "sxm.hpp &sxmDelete" c_sxmDelete :: FunPtr (Ptr SXMRaw -> IO ())

------------------- create symbolic -------------------------------
foreign import ccall unsafe "sxm.hpp createSymbolic" c_sxmCreateSymbolic :: CInt -> CInt -> Ptr CChar -> IO (Ptr SXMRaw)
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
foreign import ccall unsafe "sxm.hpp newDouble"  c_sxmNewDouble :: CDouble -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp newInt"     c_sxmNewInt :: CInt -> IO (Ptr SXMRaw)

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
foreign import ccall unsafe "sxm.hpp sxmShow"    c_sxmShow :: Ptr CChar -> CInt -> (Ptr SXMRaw) -> IO ()
sxmShow :: SXM -> IO String
sxmShow (SXM s) = do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxmShow stringRef (fromIntegral stringLength)
  peekCString stringRef

--------- size -------
foreign import ccall unsafe "sxm.hpp sxmSize1"    c_sxmSize1 :: Ptr SXMRaw -> IO CInt
foreign import ccall unsafe "sxm.hpp sxmSize2"    c_sxmSize2 :: Ptr SXMRaw -> IO CInt

-- | get (rows,cols)
sxmSize :: SXM -> IO (Int,Int)
sxmSize (SXM sxm) = mask_ $ do
  s1 <- withForeignPtr sxm c_sxmSize1
  s2 <- withForeignPtr sxm c_sxmSize2
  return (fromIntegral s1, fromIntegral s2)

-- | look up scalar
foreign import ccall unsafe "sxm.hpp sxmAt" c_sxmAt :: Ptr SXMRaw -> IO (Ptr SXMRaw)
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

foreign import ccall unsafe "sxm.hpp sxmVertCat" c_sxmVertCat :: Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp sxmHorzCat" c_sxmHorzCat :: Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXMRaw)
sxmVertCat, sxmHorzCat :: [SXM] -> IO SXM
sxmVertCat = wrapSXMList c_sxmVertCat
sxmHorzCat = wrapSXMList c_sxmHorzCat
