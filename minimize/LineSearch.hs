module LineSearch where

import Functions
import Debug.Trace

badminimize f guess = loop f guess
  where
    tol = 0.001
    loop f x = 
      let df = d f x in
      if abs(df) < tol then x else
        let alpha = strongWolfe (chooseLine (f . (+ x)) 0) in
        loop f (if df > 0 then x - alpha else x + alpha)


-- returns a scalar function of one variable
chooseLine f x = 
  if d f x > 0 then
    f . negate
  else
    f

strongWolfe phi = loop 1 alpha1 alpha0
  where
    alpha0 = 0
    alpha1 = 1.0
    alphamax = 22
    mu1 = 0.0001
    mu2 = 0.9
    dphi0 = d phi 0
    loop i alpha oldalpha = 
      if phi alpha > phi 0 + mu1 * alpha * dphi0 ||
         (phi alpha > phi oldalpha && i > 1)
      then 
        zoom oldalpha alpha
      else
        let dphi = d phi alpha in
        {-trace (show (phi alpha) ++ "/" ++ show (dphi0) ++ "\n"
                   ++ show dphi ++ "/" ++ show (-mu2*dphi0)) $-}
        if abs dphi <= -mu2 * dphi0 then alpha else
          if dphi >= 0 then zoom oldalpha alpha else
{-            trace (show alpha) $-} loop (i+1) ((alpha+alphamax) / 2) alpha

    zoom a b = 
      let alpha = (a+b)/2
          dphi = d phi alpha
      in
        if phi alpha > phi 0 + mu1 * alpha * dphi0 ||
           phi alpha > phi a then zoom a alpha
        else
          if abs dphi <= -mu2 * dphi0 then alpha else
            if dphi * (b - a) >= 0 then zoom a alpha else zoom alpha b


backtracking phi = loop alpha0
  where
    alpha0 = 1
    mu1 = 0.0001
    rho = 1.1
    loop alpha = 
      if phi alpha < phi 0 + mu1*alpha*(d phi 0) then alpha else
        loop $ rho * alpha

quad x = (x-5)^2
quart x = x^4 + x^3-22*x^2+3*x+8