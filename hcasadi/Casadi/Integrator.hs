{-# OPTIONS_GHC -Wall #-}

module Casadi.Integrator ( createCvodesIntegrator
                         , createIdasIntegrator
                         ) where

import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( newForeignPtr, withForeignPtr )
import Casadi.Bindings.Integrator
import Casadi.Bindings.SXFunction

import Casadi.SXFunction
import Casadi.SXFunctionOptions
import Casadi.SXFunctionOptionsInternal ( sxFunctionUnsafeSetOption )
import Casadi.Types

createCvodesIntegrator :: SXFunction -> [SXFunctionOption] -> IO SXFunction
createCvodesIntegrator = createIntegrator c_createCvodes

createIdasIntegrator :: SXFunction -> [SXFunctionOption] -> IO SXFunction
createIdasIntegrator = createIntegrator c_createIdas

createIntegrator :: (Ptr SXFunctionRaw -> IO (Ptr SXFunctionRaw))
                    -> SXFunction -> [SXFunctionOption] -> IO SXFunction
createIntegrator c_createIntegrator (SXFunction fun) options = do
  funRaw <- withForeignPtr fun c_createIntegrator >>= newForeignPtr c_sxFunctionDelete
  let ret = SXFunction funRaw
  mapM_ (sxFunctionUnsafeSetOption ret) options
  withForeignPtr funRaw c_sxFunctionInit

  return ret
