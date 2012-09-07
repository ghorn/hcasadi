{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.Bindings.SXFunction ( SXFunctionRaw
                                  , c_sxFunctionCreate
                                  , c_sxFunctionDelete
                                  , c_sxFunctionNumInputs
                                  , c_sxFunctionNumOutputs
                                  , c_sxFunctionInputSize
                                  , c_sxFunctionInputSize1
                                  , c_sxFunctionInputSize2
                                  , c_sxFunctionOutputSize
                                  , c_sxFunctionOutputSize1
                                  , c_sxFunctionOutputSize2
                                  , c_sxFunctionEvalDouble
                                  , c_sxFunctionSetOptionDouble
                                  , c_sxFunctionSetOptionString
                                  , c_sxFunctionSetOptionInt
                                  , c_sxFunctionSetOptionBool
                                  ) where

import Foreign.C ( CDouble(..), CInt(..), CChar(..) )
import Foreign.Ptr ( FunPtr, Ptr )

import Casadi.Bindings.SXM

data SXFunctionRaw

foreign import ccall unsafe "sxFunctionCreate" c_sxFunctionCreate
  :: Ptr (Ptr SXMRaw) -> CInt -> Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "&sxFunctionDelete" c_sxFunctionDelete
  :: FunPtr (Ptr SXFunctionRaw -> IO ())

--------------------- getters -----------------------
foreign import ccall unsafe "sxFunction.hpp sxFunctionNumInputs" c_sxFunctionNumInputs
  :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionNumOutputs" c_sxFunctionNumOutputs
  :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionInputSize" c_sxFunctionInputSize
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionInputSize1" c_sxFunctionInputSize1
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionInputSize2" c_sxFunctionInputSize2
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionOutputSize" c_sxFunctionOutputSize
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionOutputSize1" c_sxFunctionOutputSize1
  :: CInt -> Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionOutputSize2" c_sxFunctionOutputSize2
  :: CInt -> Ptr SXFunctionRaw -> IO CInt

----------------- evaluate -----------------
foreign import ccall unsafe "sxFunction.hpp sxFunctionEvalDouble" c_sxFunctionEvalDouble
  :: CInt -> Ptr (Ptr CDouble) -> Ptr CInt ->
     CInt -> Ptr (Ptr CDouble) -> Ptr CInt ->
     Ptr SXFunctionRaw -> IO CInt

----------------- set options -----------------
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionDouble" c_sxFunctionSetOptionDouble
  :: Ptr CChar -> CDouble -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionString" c_sxFunctionSetOptionString
  :: Ptr CChar -> Ptr CChar -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionInt" c_sxFunctionSetOptionInt
  :: Ptr CChar -> CInt -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionBool" c_sxFunctionSetOptionBool
  :: Ptr CChar -> CInt -> Ptr SXFunctionRaw -> IO ()

--foreign import ccall unsafe "generateCCode" c_generateCCode
--  :: Ptr CChar -> Ptr SXFunctionRaw -> IO CDouble
--foreign import ccall unsafe "createExternalFunction" c_createExternalFunction
--  :: Ptr CChar -> IO (Ptr SXFunctionRaw)

