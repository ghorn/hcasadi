{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Casadi.Bindings.SXFunction ( SXFunctionRaw
                                  , c_sxFunctionCreate
                                  , c_sxFunctionDelete
                                  , c_sxFunctionInit
                                  , c_sxFunctionNumInputs
                                  , c_sxFunctionNumOutputs
                                  , c_sxFunctionInputSize
                                  , c_sxFunctionInputSize1
                                  , c_sxFunctionInputSize2
                                  , c_sxFunctionOutputSize
                                  , c_sxFunctionOutputSize1
                                  , c_sxFunctionOutputSize2
                                  , c_sxFunctionSetInput
                                  , c_sxFunctionGetOutput
                                  , c_sxFunctionEvalDouble
                                  , c_sxFunctionSetOptionDouble
                                  , c_sxFunctionSetOptionString
                                  , c_sxFunctionSetOptionDoubleList
                                  , c_sxFunctionSetOptionIntList
                                  , c_sxFunctionSetOptionStringList
                                  , c_sxFunctionSetOptionInt
                                  , c_sxFunctionSetOptionBool
                                  ) where

import Foreign.C ( CDouble(..), CInt(..), CChar(..) )
import Foreign.Ptr ( FunPtr, Ptr )

import Casadi.Bindings.SXM

data SXFunctionRaw

foreign import ccall unsafe "sxFunction.hpp sxFunctionCreate" c_sxFunctionCreate
  :: Ptr (Ptr SXMRaw) -> CInt ->
     Ptr (Ptr SXMRaw) -> CInt -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "sxFunction.hpp &sxFunctionDelete" c_sxFunctionDelete
  :: FunPtr (Ptr SXFunctionRaw -> IO ())
foreign import ccall unsafe "sxFunction.hpp sxFunctionInit" c_sxFunctionInit
  :: Ptr SXFunctionRaw -> IO ()

--------------------- getters -----------------------
foreign import ccall unsafe "sxFunction.hpp sxFunctionGetNumInputs" c_sxFunctionNumInputs
  :: Ptr SXFunctionRaw -> IO CInt
foreign import ccall unsafe "sxFunction.hpp sxFunctionGetNumOutputs" c_sxFunctionNumOutputs
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
foreign import ccall unsafe "sxFunction.hpp sxFunctionGetOutput" c_sxFunctionGetOutput
  :: CInt -> CInt -> Ptr CDouble -> Ptr SXFunctionRaw -> IO CInt

--------------------- setters -----------------------
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetInput" c_sxFunctionSetInput
  :: CInt -> CInt -> Ptr CDouble -> Ptr SXFunctionRaw -> IO CInt

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
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionDoubleList" c_sxFunctionSetOptionDoubleList
  :: Ptr CChar -> CInt -> Ptr CDouble -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionIntList" c_sxFunctionSetOptionIntList
  :: Ptr CChar -> CInt -> Ptr CInt -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionStringList" c_sxFunctionSetOptionStringList
  :: Ptr CChar -> CInt -> Ptr (Ptr CChar) -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionInt" c_sxFunctionSetOptionInt
  :: Ptr CChar -> CInt -> Ptr SXFunctionRaw -> IO ()
foreign import ccall unsafe "sxFunction.hpp sxFunctionSetOptionBool" c_sxFunctionSetOptionBool
  :: Ptr CChar -> CInt -> Ptr SXFunctionRaw -> IO ()

--foreign import ccall unsafe "sxFunction.hpp generateCCode" c_generateCCode
--  :: Ptr CChar -> Ptr SXFunctionRaw -> IO CDouble
--foreign import ccall unsafe "sxFunction.hpp createExternalFunction" c_createExternalFunction
--  :: Ptr CChar -> IO (Ptr SXFunctionRaw)

