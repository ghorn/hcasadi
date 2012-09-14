{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}

module Casadi.Dvda ( ToCasadi(..)
                   , toSXFunction
                   ) where

import Data.Functor.Compose ( Compose(..) )
import Data.Vector.Storable ( Vector )
import Foreign.C ( CDouble )
import Control.Monad ( foldM )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Maybe ( fromMaybe )
import Data.Traversable ( Traversable )

import Dvda.Expr ( Expr, GExpr(..), Nums(..), Fractionals(..), Floatings(..) )
import Dvda.FunGraph ( FunGraph(..), exprsToFunGraph, topSort )
import Dvda.HList ( (:*)(..) )

import Casadi.SXM
import Casadi.SXFunction
import Casadi.SXFunctionOptions
import Casadi.Math

class ToCasadi a where
  type ToCasadiT a
  toCasadi :: a -> IO (ToCasadiT a)

instance ToCasadi (Expr Double) where
  type ToCasadiT (Expr Double) = SXM
  toCasadi x = fmap head $ toCasadi [x]

instance Traversable f => ToCasadi (f (Expr Double)) where
  type ToCasadiT (f (Expr Double)) = f SXM
  toCasadi = toCasadi'

instance (Traversable f, Traversable g) => ToCasadi (f (g (Expr Double))) where
  type ToCasadiT (f (g (Expr Double))) = f (g SXM)
  toCasadi x = fmap getCompose $ toCasadi (Compose x)

instance (Traversable f, Traversable g, Traversable h) => ToCasadi (f (g (h (Expr Double)))) where
  type ToCasadiT (f (g (h (Expr Double)))) = f (g (h SXM))
  toCasadi x = fmap getCompose $ toCasadi (Compose x)

instance (Traversable f, Traversable g, Traversable h, Traversable i) => ToCasadi (f (g (h (i (Expr Double))))) where
  type ToCasadiT (f (g (h (i (Expr Double))))) = f (g (h (i SXM)))
  toCasadi x = fmap getCompose $ toCasadi (Compose x)

instance (Traversable f, Traversable g, Traversable h, Traversable i, Traversable j) => ToCasadi (f (g (h (i (j (Expr Double)))))) where
  type ToCasadiT (f (g (h (i (j (Expr Double)))))) = f (g (h (i (j SXM))))
  toCasadi x = fmap getCompose $ toCasadi (Compose x)

instance (Traversable f, Traversable g, Traversable h, Traversable i, Traversable j, Traversable k) => ToCasadi (f (g (h (i (j (k (Expr Double))))))) where
  type ToCasadiT (f (g (h (i (j (k (Expr Double))))))) = f (g (h (i (j (k SXM)))))
  toCasadi x = fmap getCompose $ toCasadi (Compose x)

instance (Traversable f, Traversable g, Traversable h, Traversable i, Traversable j, Traversable k, Traversable l) => ToCasadi (f (g (h (i (j (k (l (Expr Double)))))))) where
  type ToCasadiT (f (g (h (i (j (k (l (Expr Double)))))))) = f (g (h (i (j (k (l SXM))))))
  toCasadi x = fmap getCompose $ toCasadi (Compose x)

-- turn Dvda inputs/outputs into native casadi nodes,
-- using casadi functions to appropriately return casadi scalars/vectors/matrices
toCasadi' :: Traversable f => f (Expr Double) -> IO (f SXM)
toCasadi' ins = exprsToFunGraph ins >>= fgToCasadi

toSXFunction :: (Traversable f, Traversable g) =>
                f (Expr Double) -> g (Expr Double) -> [SXFunctionOption]
                -> IO (f (Vector CDouble) -> IO (g (Vector CDouble)))
toSXFunction ins outs options = do
  (sxIns :* sxOuts) <- toCasadi (ins :* outs)
  sxFunctionCreateCallable' sxIns sxOuts options

-- turn a FunGraph into native casadi nodes (all operations scalar)
fgToCasadi :: Functor g => FunGraph Double [] g -> IO (g SXM)
fgToCasadi fg = do
  let mkCasadi :: IntMap SXM -> Int -> IO (IntMap SXM)
      mkCasadi im k
        | k `IM.member` im = error $ "toCasadi tried to insert node " ++ show k ++ " ("++show gexpr++") twice"
        | otherwise = do
          sxm <- gexprToCasadi im gexpr
          return $ IM.insert k sxm im
        where
          gexpr = fromMaybe err (fgLookupGExpr fg k)
            where
              err = error $ "fgLookupGExpr "++show k++" returned Nothing"
        
  nodes <- foldM mkCasadi IM.empty (reverse $ topSort fg)

  let outputs = (fmap f) (fgOutputs fg)
        where
          f :: Int -> SXM
          f k = fromMaybe (error msg) $ IM.lookup k nodes
            where
              msg = "fgToCasadi: output " ++ show k ++ " not found in node graph"

  return outputs

binary :: IntMap SXM -> (SXM -> SXM -> IO SXM) -> Int -> Int -> IO SXM
binary im op kx ky = case (IM.lookup kx im, IM.lookup ky im) of
  (Just x', Just y') -> op x' y'
  (Nothing, _) -> error $ "binary: intmap doesn't have node " ++ show kx
  (_, Nothing) -> error $ "binary: intmap doesn't have node " ++ show ky

unary :: IntMap SXM -> (SXM -> IO SXM) -> Int -> IO SXM
unary im op k = op $ fromMaybe err $ IM.lookup k im
  where
    err = error $ "unary: intmap doesn't have node " ++ show k

gexprToCasadi :: IntMap SXM -> GExpr Double Int -> IO SXM
gexprToCasadi _ (GSym s) = sxmSym (show s)
gexprToCasadi _ (GConst c) = sxmNewDouble c
gexprToCasadi _ (GFractional (FromRational x)) = sxmNewDouble (fromRational x)
gexprToCasadi _ (GNum (FromInteger x)) = sxmNewIntegral x
gexprToCasadi im (GNum (Mul x y))          = binary im sxmTimes x y
gexprToCasadi im (GNum (Add x y))          = binary im sxmPlus x y
gexprToCasadi im (GNum (Sub x y))          = binary im sxmMinus x y
gexprToCasadi im (GNum (Negate x))         = unary im sxmNegate x
gexprToCasadi im (GNum (Abs x))            = unary im sxmAbs x
gexprToCasadi im (GFractional (Div x y))   = binary im sxmDivide x y
gexprToCasadi im (GFloating (Pow x y))     = binary im sxmPow x y
gexprToCasadi im (GFloating (Exp x))       = unary im sxmExp   x
gexprToCasadi im (GFloating (Log x))       = unary im sxmLog   x
gexprToCasadi im (GFloating (Sin x))       = unary im sxmSin   x
gexprToCasadi im (GFloating (Cos x))       = unary im sxmCos   x
gexprToCasadi im (GFloating (ASin x))      = unary im sxmArcsin  x
gexprToCasadi im (GFloating (ATan x))      = unary im sxmArctan  x
gexprToCasadi im (GFloating (ACos x))      = unary im sxmArccos  x
gexprToCasadi im (GFloating (Sinh x))      = unary im sxmSinh  x
gexprToCasadi im (GFloating (Cosh x))      = unary im sxmCosh  x
gexprToCasadi im (GFloating (Tanh x))      = unary im sxmTanh  x
gexprToCasadi _ (GNum (Signum _)) = error "gexprToCasadi: no casadi \"signum\" available"
gexprToCasadi _ (GFloating (ASinh _)) = error "gexprToCasadi: no casadi \"asinh\" available"
gexprToCasadi _ (GFloating (ATanh _)) = error "gexprToCasadi: no casadi \"atanh\" available"
gexprToCasadi _ (GFloating (ACosh _)) = error "gexprToCasadi: no casadi \"acosh\" available"
gexprToCasadi _ (GFloating (LogBase _ _)) = error "gexprToCasadi: no casadi \"logBase\" available"
