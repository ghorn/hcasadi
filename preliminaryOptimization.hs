-- preliminaryOptimization.hs

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Foreign.Storable
import Numeric.LinearAlgebra

rosenbrockF :: (Storable a, Num a) => Vector a -> a
rosenbrockF x = (1-x0)*(1-x0) + 100*(x1-x0*x0)*(x1-x0*x0)
  where
    [x0,x1] = toList x

rosenbrockG :: (Storable a, Num a) => Vector a -> Vector a
rosenbrockG x = fromList [2*(x0-1) + 400*x0*(x0*x0-x1),
                          200*(x1-x0*x0)]
  where
    [x0,x1] = toList x

tau :: Floating a => a
tau = 2/(1+sqrt(5))

goldenSectionSearch :: (Ord a, Floating b) => (b -> a) -> (b, b) -> [(b,a)]
goldenSectionSearch f (x0,x3) = gss f (x0,x1,x2,x3)
  where
    x1 = x0 + (x3-x0)*(1-tau)
    x2 = x0 + (x3-x0)*tau
    
gss :: (Ord a, Floating b) => (b -> a) -> (b, b, b, b) -> [(b,a)]
gss f (x0, x1, x2, x3) = xs
  where
    x1' = x0 + (x2-x0)*(1-tau)
    x2' = x1 + (x3-x1)*tau
    xs
      | (f x1) < (f x2) = (x1, f x1):(gss f (x0,x1',x1,x2))
      | otherwise       = (x2, f x2):(gss f (x1,x2,x2',x3))

dfp :: (Product e, Container Vector e, Ord t, Num (Vector e), Floating e) =>
       Vector e -> (Vector e -> t) -> (Vector e -> Vector e) -> [(Vector e, Matrix e)]
dfp x0 f g = iterate (\(x,v) -> oneDfp x v f g) (x0, v0)
  where
    v0 = ident $ fromIntegral $ length (toList x0)

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
    v0 = ident $ fromIntegral $ length (toList x0)

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

    
main :: IO ()
main = do
  print "hi"
  let x0 = fromList [-1.2,1] :: Vector Double
--      results = (take 20 $ dfp x0 rosenbrockF rosenbrockG)
      results = (take 20 $ bfgs x0 rosenbrockF rosenbrockG)
  mapM_ (\(_,v) -> print $ inv v) results
  mapM_ (\(x,_) -> print x) results
