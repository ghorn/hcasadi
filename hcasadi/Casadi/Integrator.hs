{-# OPTIONS_GHC -Wall #-}

module Casadi.Integrator ( createCvodesIntegrator
                         , createIdasIntegrator
                         ) where

import Data.Maybe ( isJust, isNothing )
import Data.Vector.Storable ( Vector )
import Foreign.C ( CDouble )
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( newForeignPtr, withForeignPtr )

import Casadi.Bindings.Integrator
import Casadi.Bindings.SXFunction

import Casadi.DAE
import Casadi.SXFunction
import Casadi.SXFunctionOptions
import qualified Casadi.IdasOptions as Idas
import qualified Casadi.CvodesOptions as Cvodes
import Casadi.IntegratorOptionsInternal ( idasUnsafeSetOption, cvodesUnsafeSetOption )

import Casadi.Types

createCvodesIntegrator ::
  DAEIn (Maybe SXM) -> DAEOut (Maybe SXM)
  -> [SXFunctionOption] -> [Cvodes.CvodesOption]
  -> IO (Vector CDouble -> Maybe (Vector CDouble) -> IO (Vector CDouble))
createCvodesIntegrator = createIntegrator c_createCvodes cvodesUnsafeSetOption

createIdasIntegrator ::
  DAEIn (Maybe SXM) -> DAEOut (Maybe SXM)
  -> [SXFunctionOption] -> [Idas.IdasOption]
  -> IO (Vector CDouble -> Maybe (Vector CDouble) -> IO (Vector CDouble))
createIdasIntegrator = createIntegrator c_createIdas idasUnsafeSetOption

createIntegrator :: (Ptr SXFunctionRaw -> IO (Ptr SXFunctionRaw))
                    -> (SXFunction -> a -> IO ())
                    -> DAEIn (Maybe SXM) -> DAEOut (Maybe SXM)
                    -> [SXFunctionOption] -> [a]
                    -> IO (Vector CDouble -> Maybe (Vector CDouble) -> IO (Vector CDouble))
createIntegrator c_createIntegrator integratorUnsafeSetOption daeIn daeOut funOptions integratorOptions
  | isNothing (dae_X daeIn)    = error "createIntegrator: must set dae_X in DAEIn"
  | isNothing (dae_XDOT daeIn) = error "createIntegrator: must set dae_XDOT in DAEIn"
  | isNothing (dae_ODE daeOut) = error "createIntegrator: must set dae_ODE in DAEOut"
  | otherwise = do
    (SXFunction ffcn) <- sxFunctionCreate daeIn daeOut funOptions
    
    funRaw <- withForeignPtr ffcn c_createIntegrator >>= newForeignPtr c_sxFunctionDelete
    let fun = SXFunction funRaw
    mapM_ (integratorUnsafeSetOption fun) integratorOptions
    withForeignPtr funRaw c_sxFunctionInit

    f <- sxFunctionMakeCallable fun [dae_X daeIn, dae_P daeIn, Nothing] [dae_X daeIn, Nothing, Nothing, Nothing]
    let hasParams = isJust (dae_P daeIn)
        callIntegrator _ Nothing
          | hasParams = error "integrator needs parameters"
        callIntegrator _ (Just _)
          | not hasParams = error "integrator was passed parameters but the system has no parameters"
        callIntegrator x0 params = do
          Just xfinal <- fmap head $ f [Just x0, params, Nothing]
          return xfinal

    return callIntegrator
