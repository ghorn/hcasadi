module Functions where

import Data.Packed.Vector
import Numeric.GSL.Differentiation
import Numeric.Container
import Numeric.LinearAlgebra.Algorithms hiding (hess)

type FunctionVS = ((Vector Double) -> Double)
type FunctionVV = ((Vector Double) -> (Vector Double))
type FunctionSS = (Double -> Double)

d :: FunctionSS -> FunctionSS
d f = fst . (derivCentral 0.001 f)

dn :: Int -> FunctionVS -> FunctionVS
dn i f p =
  let val = p @> i 
      [a,b] = takesV [i+1, dim p-i-1] p in
  fst $ derivCentral 0.01 (\x -> f $ join $ [subVector 0 i a, fromList [x], b]) val

grad :: FunctionVS -> FunctionVV
grad f p = fromList [dn i f p | i <- [0..dim p - 1]]

hess :: FunctionVS -> (Vector Double) -> Matrix Double
hess f = \v -> buildMatrix (dim v) (dim v) (\(i,j) -> dn i (dn j f) v)


-- some test stufff
a = buildMatrix 3 3 (\(a,b) -> if a == b then 1.0 else 0.0 :: Double)

fun :: Vector Double -> Double
fun = (\b -> b <.> b)

x = fromList [0,1,2,3,4] :: Vector Double

origin = fromList [0,0,0] :: Vector Double
