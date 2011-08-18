import LineSearch
import Functions
import Data.Packed.Vector
import Numeric.GSL.Differentiation
import Numeric.Container
import Numeric.LinearAlgebra.Algorithms hiding (hess)
import Numeric.LinearAlgebra hiding(hess)

import Debug.Trace
import Graphics.Gnuplot.Simple


--minimize :: FunctionVS -> Vector Double -> [Vector Double]
minimize f grad x0 = loop 0 f x0 i i (grad f x0) []
  where
    n = dim x0
    i = ident n
    epsilon = 1e-4
    loop index f x v v2 g sol = 
      let p = -v <> g
          s = scale alpha p
          alpha = strongWolfe (\alpha -> f (x + scale alpha p))
          x' = x + s
          g' = grad f x'
          y = g' - g
          v'' = bfgsV v
          v2'' = smV v
          v' = if mod index 44 == 0 then i else v''
          v2' = if mod index 44 == 0 then i else v2''
          scl = scale (1/(s <.> y))          
          bfgsV v =  (i - scl (s `outer` y)) * v *
                     (i - scl (y `outer` s)) + 
                         (scl (s `outer` s))

          smV v = v + scale (1/(s <.> y)^2) (scale (s <.> y + y <.> (v <> y)) (s `outer` s))
                    - scale (1/(s <.> y)) (v <> (y `outer` s) + (s `outer` y) <> v)
               
      in
        if g <.> g <  epsilon || index > 500 then x : sol else
          trace (concatMap show [v, v2]) $ loop (index + 1) f x' v' v2' g' (x : sol)


plotSolution xs = 
  plotPath [] $ map (\v -> (v @> 0, v @> 1)) xs
                

rosen v = let a = v @> 0
              b = v @> 1 in
          100*(b-a^2)^2 + (1-a)^2

rgrad _ v = let a = v @> 0
                b = v @> 1 in
          fromList
              [-400*(b-a^2)*a - 2*(1-a), 
               200*(b-a^2)]

vec xs = fromList xs :: Vector Double

qquad v = let a = v @> 0
              b = v @> 1 in
          (a-1)^2 + 10*(b-2)^2