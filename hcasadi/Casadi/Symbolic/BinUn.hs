{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language GADTs #-}
{-# Language ConstraintKinds #-}
--{-# Language FlexibleContexts #-}
--{-# Language TypeSynonymInstances #-}
--{-# Language FlexibleInstances #-}
-- {-# MultiParamTypeClasses, FlexibleInstances #-}

module Casadi.Symbolic.BinUn( BinOp(..)
                            , UnOp(..)
                            , showBinary
                            , showUnary
                            , casadiApplyUnary
                            , casadiApplyBinary
                            ) where

import Casadi.Bindings.SXM

data UnOp = Abs
          | Neg
          | Signum
          | Sqrt
          | Exp
          | Log
          | Sin
          | Cos
          | Tan
          | ASin
          | ATan
          | ACos
          | Sinh
          | Cosh
          | ASinh
          | ATanh
          | ACosh deriving (Eq, Show)

showUnary :: Show a => a -> UnOp -> String
showUnary x Abs    = "|" ++ show x ++ "|"
showUnary x Neg    = "-"++paren x
showUnary x Signum = "signum"++paren x
showUnary x Exp    = "exp"++paren x
showUnary x Sqrt   = "sqrt"++paren x
showUnary x Log    = "log"++paren x
showUnary x Sin    = "sin"++paren x
showUnary x Cos    = "cos"++paren x
showUnary x Tan    = "tan"++paren x
showUnary x ASin   = "asin"++paren x
showUnary x ACos   = "acos"++paren x
showUnary x ATan   = "atan"++paren x
showUnary x Sinh   = "sinh"++paren x
showUnary x Cosh   = "cosh"++paren x
showUnary x ASinh  = "asinh"++paren x
showUnary x ATanh  = "atanh"++paren x
showUnary x ACosh  = "acosh"++paren x

casadiApplyUnary :: UnOp -> SXM -> IO SXM
casadiApplyUnary Abs    = sxAbs
casadiApplyUnary Neg    = sxNegate
--casadiApplyUnary Signum = sxSignum
casadiApplyUnary Exp    = sxExp
casadiApplyUnary Sqrt   = sxSqrt
casadiApplyUnary Log    = sxLog
casadiApplyUnary Sin    = sxSin
casadiApplyUnary Cos    = sxCos
casadiApplyUnary Tan    = sxTan
casadiApplyUnary ASin   = sxArcsin
casadiApplyUnary ACos   = sxArccos
casadiApplyUnary ATan   = sxArctan
--casadiApplyUnary Sinh   = "sinh"++paren x
--casadiApplyUnary Cosh   = "cosh"++paren x
--casadiApplyUnary ASinh  = "asinh"++paren x
--casadiApplyUnary ATanh  = "atanh"++paren x
--casadiApplyUnary ACosh  = "acosh"++paren x

casadiApplyBinary :: BinOp -> SXM -> SXM -> IO SXM
casadiApplyBinary Add = sxPlus
casadiApplyBinary Sub = sxMinus
casadiApplyBinary Mul = sxTimes
casadiApplyBinary Div = sxDivide
casadiApplyBinary Pow = sxPow

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Pow deriving (Eq, Show)

showBinary :: BinOp -> String
showBinary Add = "+"
showBinary Sub = "-"
showBinary Mul = "*"
showBinary Div = "/"

paren :: Show a => a -> String
paren x = "( "++show x++" )"
