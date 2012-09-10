{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Casadi.Bindings.Integrator ( c_createCvodes
                                  , c_createIdas
                                  ) where

import Foreign.Ptr ( Ptr )

import Casadi.Bindings.SXFunction ( SXFunctionRaw )

foreign import ccall unsafe "integrator.hpp createCvodes" c_createCvodes ::
  Ptr SXFunctionRaw -> IO (Ptr SXFunctionRaw)
foreign import ccall unsafe "integrator.hpp createIdas" c_createIdas ::
  Ptr SXFunctionRaw -> IO (Ptr SXFunctionRaw)
