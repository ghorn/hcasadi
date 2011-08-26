-- Api.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Casadi.Api
       (
         SX(..)
       , SXMatrix(..)
       , sxSymbolic
       , sxMatrixSymbolic
       , sxInt
       , sxDouble
       , trans
       , (<>)
       , fromList
       , toList
       , toLists
       , dim
       , rows
       , cols
       , concatMat
       ) where

import Casadi.SX
import Casadi.SXMatrix
import System.IO.Unsafe(unsafePerformIO)

class Mul a b c | a b -> c where
  (<>) :: a -> b -> c

instance Mul SXMatrix SXMatrix SXMatrix where
  (<>) a b = a*b

instance Mul SX SXMatrix SXMatrix where
  (<>) s m = sxMatrixScale s m
instance Mul SXMatrix SX SXMatrix where
  (<>) m s = s <> m

--instance Mul Double SXMatrix SXMatrix where
--  (<>) d m = sxMatrixScale (sxDouble d) m
--instance Mul SXMatrix Double SXMatrix where
--  (<>) m d = d <> m
--
--instance Mul Int SXMatrix SXMatrix where
--  (<>) i m = (sxInt i) <> m
--instance Mul SXMatrix Int SXMatrix where
--  (<>) m i = i <> m


trans :: SXMatrix -> SXMatrix
trans = sxMatrixTranspose

sxSymbolic :: String -> SX
sxSymbolic name = unsafePerformIO $ do
  sym <- sxCreateSymbolic name
  return sym

sxMatrixSymbolic :: String -> (Int,Int) -> SXMatrix
sxMatrixSymbolic prefix dim' = unsafePerformIO $ do
  mat <- sxMatrixCreateSymbolic prefix dim'
  return mat

sxInt :: Int -> SX
sxInt val = unsafePerformIO $ do
  s <- sxNewInt val
  return s

--sxIntegral :: Integral a => a -> SX
--sxIntegral val = unsafePerformIO $ do
--  s <- sxNewIntegral val
--  return s

sxDouble :: Double -> SX
sxDouble val = unsafePerformIO $ do
  s <- sxNewDouble val
  return s

toList :: SXMatrix -> [SX]
toList = sxMatrixToList

toLists :: SXMatrix -> [[SX]]
toLists = sxMatrixToLists

fromList :: [SX] -> SXMatrix
fromList = sxMatrixFromList

dim :: SXMatrix -> (Int,Int)
dim = sxMatrixSize

rows :: SXMatrix -> Int
rows = fst . dim

cols :: SXMatrix -> Int
cols = snd . dim

concatMat :: [SXMatrix] -> SXMatrix
concatMat mats = fromList $ concat $ map toList mats
