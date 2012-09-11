{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}

module Casadi.Dvda ( toCasadi
                   ) where

import Control.Monad ( foldM )
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Maybe ( fromMaybe )
import qualified Data.Traversable as Traversable

import Dvda.Expr ( GExpr(..), Nums(..), Fractionals(..), Floatings(..) )
import Dvda.FunGraph ( FunGraph(..), MVS(..), ToFunGraph(..), toFunGraph, topSort )

import Casadi.SXM
import Casadi.Math

-- turn Dvda inputs/outputs into native casadi nodes,
-- using casadi functions to appropriately return casadi scalars/vectors/matrices
toCasadi :: (ToFunGraph a, ToFunGraph b, NumT a ~ Double, NumT b ~ Double)
            => a -> b -> IO ([SXM], [SXM])
toCasadi ins outs = toFunGraph ins outs >>= fgToCasadi

-- turn a FunGraph into native casadi nodes,
-- using casadi functions to appropriately return casadi scalars/vectors/matrices
fgToCasadi :: FunGraph Double -> IO ([SXM],[SXM])
fgToCasadi fg = do
  (insMVS, outsMVS) <- fgToCasadi' fg
  let catMVS :: MVS SXM -> IO SXM
      catMVS (Sca x) = return x
      catMVS (Vec xs) = sxmVecCat xs
      catMVS (Mat xs) = do
        cols <- mapM sxmVecCat xs
        sxmHorzCat cols >>= sxmTranspose
  ins <- mapM catMVS insMVS
  outs <- mapM catMVS outsMVS
  return (ins, outs)

-- turn a FunGraph into native casadi nodes (all operations scalar)
fgToCasadi' :: FunGraph Double -> IO ([MVS SXM],[MVS SXM])
fgToCasadi' fg = do
  let mkCasadi :: (HashMap (GExpr Double Int) SXM, IntMap SXM)
                  -> Int
                  -> IO (HashMap (GExpr Double Int) SXM, IntMap SXM)
      mkCasadi (hm,im) k
        | k `IM.member` im = error $ "toCasadi tried to insert node " ++ show k ++ " ("++show gexpr++") twice"
        | otherwise = do
          sxm <- gexprToCasadi im gexpr
          let newIm = IM.insert k sxm im
              newHm = case gexpr of
                (GSym _) -> HM.insert gexpr sxm hm
                _ -> hm
          return (newHm, newIm)
        where
          gexpr = fromMaybe err (fgLookupGExpr fg k)
            where
              err = error $ "fgLookupGExpr "++show k++" returned Nothing"
        
  (gsymMap, nodes) <- foldM mkCasadi (HM.empty, IM.empty) (reverse $ topSort fg)

  let outputs = map (fmap f) (fgOutputs fg)
        where
          f :: Int -> SXM
          f k = fromMaybe (error msg) $ IM.lookup k nodes
            where
              msg = "fgToCasadi: output " ++ show k ++ " not found in node graph"

      -- if this input isn't a parent of any output, make a new SXM, otherwise return the existing SXM
      maybeNewSym :: GExpr Double Int -> IO SXM
      maybeNewSym gexpr@(GSym s) = case HM.lookup gexpr gsymMap of
        Just sxm -> return sxm
        Nothing -> sxmSym (show s)
      maybeNewSym gexpr = error $ "fgToCasadi maybeNewSym: got non-GSym: " ++ show gexpr
  inputs <- mapM (Traversable.mapM maybeNewSym) (fgInputs fg)

  return (inputs, outputs)

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
