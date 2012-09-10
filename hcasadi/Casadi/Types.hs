{-# OPTIONS_GHC -Wall #-}

module Casadi.Types ( SXFunction(..)
                    , SXM(..)
                    , NLPSolver(..)
                    , sxmShow
                    ) where

import Foreign.C ( newCStringLen, peekCString )
import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.ForeignPtr ( ForeignPtr )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Bindings.SXFunction ( SXFunctionRaw )
import Casadi.Bindings.SXM ( SXMRaw, c_sxmShow )
import Casadi.Bindings.NLPSolver ( NLPSolverRaw )

newtype SXFunction = SXFunction (ForeignPtr SXFunctionRaw)
newtype SXM = SXM (ForeignPtr SXMRaw)
newtype NLPSolver = NLPSolver (ForeignPtr NLPSolverRaw)

instance Show SXM where
  {-# NOINLINE show #-}
  show s = unsafePerformIO $ sxmShow s

-- only put this show here so that we can avoid orphan instances
sxmShow :: SXM -> IO String
sxmShow (SXM s) = do
  (stringRef, stringLength) <- newCStringLen $ replicate 512 ' '
  withForeignPtr s $ c_sxmShow stringRef (fromIntegral stringLength)
  peekCString stringRef

