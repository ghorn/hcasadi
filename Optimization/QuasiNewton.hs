-- QuasiNewton.hs

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Optimization.QuasiNewton(dfp, bfgs) where

import Optimization.LineSearch
import Numeric.LinearAlgebra

type HessianApprox a = Matrix a -> Vector a -> Vector a -> Vector a -> Vector a -> Matrix a


dfp :: (Product e, Container Vector e, Ord e, Num (Vector e), Floating e) =>
       Vector e -> (Vector e -> e) -> (Vector e -> Vector e) -> [(Vector e, Matrix e)]
dfp = quasiNewton dfpHessianApprox goldenSectionSearch


bfgs :: (Product e, Container Vector e, Ord e, Num (Vector e), Floating e) =>
       Vector e -> (Vector e -> e) -> (Vector e -> Vector e) -> [(Vector e, Matrix e)]
bfgs = quasiNewton bfgsHessianApprox goldenSectionSearch


quasiNewton :: (Product a, Container Vector a, Ord a, Num (Vector a), Floating a) =>
               HessianApprox a -> LineSearch a -> Vector a -> (Vector a -> a) -> (Vector a -> Vector a) -> [(Vector a, Matrix a)]
quasiNewton hessianApprox linesearch x0 f g = iterate (\(x,v) -> oneQuasiNewton hessianApprox linesearch x v f g) (x0, v0)
  where
    v0 = ident (dim x0)


oneQuasiNewton :: (Product a, Container Vector a, Ord a, Num (Vector a), Floating a) =>
                  HessianApprox a -> LineSearch a-> Vector a -> Matrix a -> (Vector a -> a) -> (Vector a -> Vector a) -> (Vector a, Matrix a)
oneQuasiNewton hessianApprox linesearch xk vk f g = (xkp1, vkp1)
  where
    vkp1 = hessianApprox vk xk xkp1 gk gkp1
    gk   = g xk
    gkp1 = g xkp1

    pk = -( vk <> gk )
    f' alpha = f $ xk + (scale alpha pk)
    g' alpha = (g $ xk + (scale alpha pk)) <.> pk
    h' _ = error "Error in QuasiNewton - please implement directional hessian approximation"
    (alphakp1, _) = head $ drop 200 $ linesearch f' g' h' (0,10)
    xkp1 = xk + (scale alphakp1 pk)


bfgsHessianApprox :: (Product t, Container Vector t, Num (Vector t)) =>
                     Matrix t -> Vector t -> Vector t -> Vector t -> Vector t -> Matrix t
bfgsHessianApprox vk xk xkp1 gk gkp1 = vkp1
  where
    vkp1 = (eye - (scale den (outer sk yk))) <> vk <> (eye - (scale den (outer yk sk)))
           + (scale den (outer sk sk))

    eye = ident $ cols vk
    den = 1/(sk <.> yk)

    yk = gkp1 - gk
    sk = xkp1 - xk
    

dfpHessianApprox :: (Product t, Container Vector t, Num (Vector t)) =>
                     Matrix t -> Vector t -> Vector t -> Vector t -> Vector t -> Matrix t
dfpHessianApprox vk xk xkp1 gk gkp1 = vkp1
  where
    vkp1 = vk - ak + bk

    ak = scale (1/(yk <.> (vk <> yk))) (outer (vk <> yk) (yk <> vk))
    bk = scale (1/(sk <.> yk)) (outer sk sk)

    yk = gkp1 - gk
    sk = xkp1 - xk
