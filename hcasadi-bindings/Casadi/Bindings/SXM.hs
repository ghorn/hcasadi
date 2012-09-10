{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.Bindings.SXM ( SXMRaw
                           , c_sxmDelete
                           , c_sxmCreateSymbolic
                           , c_sxmNewDouble
                           , c_sxmNewInt
                           , c_sxmShow
                           , c_sxmSize1
                           , c_sxmSize2
                           , c_sxmAt
                           , c_sxmVecCat
                           , c_sxmVertCat
                           , c_sxmHorzCat
                           ) where

import Foreign.C ( CChar(..), CDouble(..), CInt(..) )
import Foreign.Ptr ( FunPtr, Ptr )

-- the SX data type
data SXMRaw

foreign import ccall unsafe "sxm.hpp &sxmDelete" c_sxmDelete :: FunPtr (Ptr SXMRaw -> IO ())
foreign import ccall unsafe "sxm.hpp createSymbolic" c_sxmCreateSymbolic :: CInt -> CInt -> Ptr CChar -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp newDouble"  c_sxmNewDouble :: CDouble -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp newInt"     c_sxmNewInt :: CInt -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp sxmShow"    c_sxmShow :: Ptr CChar -> CInt -> (Ptr SXMRaw) -> IO ()
foreign import ccall unsafe "sxm.hpp sxmSize1"    c_sxmSize1 :: Ptr SXMRaw -> IO CInt
foreign import ccall unsafe "sxm.hpp sxmSize2"    c_sxmSize2 :: Ptr SXMRaw -> IO CInt
foreign import ccall unsafe "sxm.hpp sxmAt" c_sxmAt :: Ptr SXMRaw -> CInt -> CInt -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp sxmVecCat"  c_sxmVecCat  :: Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp sxmVertCat" c_sxmVertCat :: Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXMRaw)
foreign import ccall unsafe "sxm.hpp sxmHorzCat" c_sxmHorzCat :: Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXMRaw)
