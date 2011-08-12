-- CasadiInterfaceUtils.hs

{-# OPTIONS_GHC -Wall #-}

module CasadiInterfaceUtils(withForeignPtrs2, withForeignPtrs3) where

import Foreign.Ptr
import Foreign.ForeignPtr

-- convenience functions for cpp wrappers
withForeignPtrs2 :: (Ptr a -> Ptr b -> IO c) -> ForeignPtr a -> ForeignPtr b -> IO c
withForeignPtrs2 f0 p0 p1 = withForeignPtr p1 $ \p1' -> (f1 p1')
  where
    f1 p1' = withForeignPtr p0 (\p0' -> f0 p0' p1')

withForeignPtrs3 :: (Ptr a -> Ptr b -> Ptr c -> IO d) -> ForeignPtr a -> ForeignPtr b -> ForeignPtr c -> IO d
withForeignPtrs3 f0 p0 p1 p2 = withForeignPtr p2 $ \p2' -> (f2 p2')
  where
    f2 p2'     = withForeignPtr p1 (\p1' -> f1 p1' p2'    )
    f1 p1' p2' = withForeignPtr p0 (\p0' -> f0 p0' p1' p2')
