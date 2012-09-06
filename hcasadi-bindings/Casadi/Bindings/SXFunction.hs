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
                                  ) where

import Foreign.C ( CDouble(..), CInt(..) )
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

--foreign import ccall unsafe "sxFunctionGetInputsSX" c_sxFunctionGetInputsSX
--  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionGetOutputsSX" c_sxFunctionGetOutputsSX
--  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionGradient" c_sxFunctionGradient
--  :: Ptr SXFunctionRaw -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionJacobian" c_sxFunctionJacobian
--  :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMRaw -> IO ()
--foreign import ccall unsafe "sxFunctionHessian" c_sxFunctionHessian
--  :: Ptr SXFunctionRaw -> CInt -> CInt -> Ptr SXMRaw -> IO ()
--
--foreign import ccall unsafe "generateCCode" c_generateCCode
--  :: Ptr CChar -> Ptr SXFunctionRaw -> IO CDouble
--foreign import ccall unsafe "createExternalFunction" c_createExternalFunction
--  :: Ptr CChar -> IO (Ptr SXFunctionRaw)

