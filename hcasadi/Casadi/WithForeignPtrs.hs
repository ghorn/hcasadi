{-# OPTIONS_GHC -Wall #-}

module Casadi.WithForeignPtrs ( withForeignPtrs2
                              , withForeignPtrs3
                              ) where

import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, touchForeignPtr )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

-- convenience functions for cpp wrappers
withForeignPtrs2 :: ForeignPtr a -> ForeignPtr b -> (Ptr a -> Ptr b -> IO c) -> IO c
withForeignPtrs2 p0' p1' f = do
  let p0 = unsafeForeignPtrToPtr p0'
      p1 = unsafeForeignPtrToPtr p1'
  ret <- f p0 p1
  touchForeignPtr p0'
  touchForeignPtr p1'
  return ret

withForeignPtrs3 :: ForeignPtr a -> ForeignPtr b -> ForeignPtr c -> (Ptr a -> Ptr b -> Ptr c -> IO d) -> IO d
withForeignPtrs3 p0' p1' p2' f = do
  let p0 = unsafeForeignPtrToPtr p0'
      p1 = unsafeForeignPtrToPtr p1'
      p2 = unsafeForeignPtrToPtr p2'
  ret <- f p0 p1 p2
  touchForeignPtr p0'
  touchForeignPtr p1'
  touchForeignPtr p2'
  return ret
