{-# OPTIONS_GHC -Wall #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}

module Casadi.Options ( Option(..)
                      ) where

import Foreign.C ( newCString )
import Foreign.ForeignPtr ( withForeignPtr )
import Foreign.Marshal ( newArray, free )

import Casadi.Bindings.SXFunction
import Casadi.Types ( SXFunction(..) )

class Option a where
  unsafeSetOption :: SXFunction -> String -> a -> IO ()

instance Option Double where
  unsafeSetOption (SXFunction rawFun) name' val = do
    name <- newCString name'
    withForeignPtr rawFun (c_sxFunctionSetOptionDouble name (realToFrac val))
    free name
instance Option String where
  unsafeSetOption (SXFunction rawFun) name' val' = do
    name <- newCString name'
    val <- newCString val'
    withForeignPtr rawFun (c_sxFunctionSetOptionString name val)
    free name
    free val
instance Option [Double] where
  unsafeSetOption (SXFunction rawFun) name' vals' = do
    name <- newCString name'
    valArray <- newArray (map realToFrac vals')
    withForeignPtr rawFun (c_sxFunctionSetOptionDoubleList name (fromIntegral (length vals')) valArray)
    free name
    free valArray
instance Option [String] where
  unsafeSetOption (SXFunction rawFun) name' vals' = do
    name <- newCString name'
    vals <- mapM newCString vals'
    valArray <- newArray vals
    withForeignPtr rawFun (c_sxFunctionSetOptionStringList name (fromIntegral (length vals)) valArray)
    free name
    free valArray
    mapM_ free vals
instance Option [Int] where
  unsafeSetOption (SXFunction rawFun) name' vals' = do
    name <- newCString name'
    valArray <- newArray (map fromIntegral vals')
    withForeignPtr rawFun (c_sxFunctionSetOptionIntList name (fromIntegral (length vals')) valArray)
    free name
    free valArray
instance Option Int where
  unsafeSetOption (SXFunction rawFun) name' val = do
    name <- newCString name'
    withForeignPtr rawFun (c_sxFunctionSetOptionInt name (fromIntegral val))
    free name
instance Option Bool where
  unsafeSetOption (SXFunction rawFun) name' val = do
    name <- newCString name'
    let intVal = if val then 1 else 0
    withForeignPtr rawFun (c_sxFunctionSetOptionBool name intVal)
    free name
