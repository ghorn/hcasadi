-- QuasiNewton.hs

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Optimization.QuasiNewton(dfp, bfgs) where

import Optimization.LineSearch
import Numeric.LinearAlgebra

dfp :: (Product e, Container Vector e, Ord t, Num (Vector e), Floating e) =>
       Vector e -> (Vector e -> t) -> (Vector e -> Vector e) -> [(Vector e, Matrix e)]
dfp x0 f g = iterate (\(x,v) -> oneDfp x v f g) (x0, v0)
  where
    v0 = ident (dim x0)

oneDfp :: (Product e, Container Vector e, Ord t, Num (Vector e), Floating e) =>
       Vector e -> Matrix e -> (Vector e -> t) -> (Vector e -> Vector e) -> (Vector e, Matrix e)
oneDfp xk vk f g = (xkp1, vkp1)
  where
    vkp1 = vk - ak + bk
    gk   = g xk
    gkp1 = g xkp1
    yk = gkp1 - gk
    sk = xkp1 - xk
    
    bk = scale (1/(sk <.> yk)) (outer sk sk)
    ak = scale (1/(yk <.> (vk <> yk))) (outer (vk <> yk) (yk <> vk))

    pk = -( vk <> gk )
    f' alpha = f $ xk + (scale alpha pk)
    (alphakp1, _) = head $ drop 200 $ goldenSectionSearch f' (0,10)
    xkp1 = xk + (scale alphakp1 pk)
    
bfgs :: (Product e, Container Vector e, Ord t, Num (Vector e), Floating e) =>
       Vector e -> (Vector e -> t) -> (Vector e -> Vector e) -> [(Vector e, Matrix e)]
bfgs x0 f g = iterate (\(x,v) -> oneBfgs x v f g) (x0, v0)
  where
    v0 = ident (dim x0)

oneBfgs :: (Product e, Container Vector e, Ord t, Num (Vector e), Floating e) =>
       Vector e -> Matrix e -> (Vector e -> t) -> (Vector e -> Vector e) -> (Vector e, Matrix e)
oneBfgs xk vk f g = (xkp1, vkp1)
  where
    eye = ident $ cols vk
    den = 1/(sk <.> yk)
    
    vkp1 = (eye - (scale den (outer sk yk))) <> vk <> (eye - (scale den (outer yk sk)))
           + (scale den (outer sk sk))
    gk   = g xk
    gkp1 = g xkp1
    yk = gkp1 - gk
    sk = xkp1 - xk
    
    pk = -( vk <> gk )
    f' alpha = f $ xk + (scale alpha pk)
    (alphakp1, _) = head $ drop 200 $ goldenSectionSearch f' (0,10)
    xkp1 = xk + (scale alphakp1 pk)
