{-# OPTIONS_GHC -Wall #-}

module Casadi.BinUn( BinOp(..)
                   , UnOp(..)
                   , showBinary
                   , showUnary
                   , casadiApplyUnary
                   , casadiApplyBinary
                   ) where

import qualified Casadi.Bindings as C
import Casadi.Bindings(SX, CasadiModule)

data UnOp = Abs
          | Neg
          | Signum
          | Sqrt
          | Exp
          | Log
          | Sin
          | Cos
          | Tan
          | Tanh
          | ASin
          | ATan
          | ACos
          | Sinh
          | Cosh
          | Trans deriving (Eq, Show)

showUnary :: Show a => a -> UnOp -> String
showUnary x Trans  = paren x ++ ".T"
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
showUnary x Tanh   = "tanh"++paren x
--showUnary x ASinh  = "asinh"++paren x
--showUnary x ATanh  = "atanh"++paren x
--showUnary x ACosh  = "acosh"++paren x

casadiApplyUnary :: UnOp -> CasadiModule -> SX -> IO SX
casadiApplyUnary Trans  = C.transpose
casadiApplyUnary Abs    = C.abs
casadiApplyUnary Neg    = C.neg
casadiApplyUnary Signum = C.signum
casadiApplyUnary Exp    = C.exp
casadiApplyUnary Sqrt   = C.sqrt
casadiApplyUnary Log    = C.log
casadiApplyUnary Sin    = C.sin
casadiApplyUnary Cos    = C.cos
casadiApplyUnary Tan    = C.tan
casadiApplyUnary ASin   = C.asin
casadiApplyUnary ACos   = C.acos
casadiApplyUnary ATan   = C.atan
casadiApplyUnary Sinh   = C.sinh
casadiApplyUnary Cosh   = C.cosh
casadiApplyUnary Tanh   = C.tanh
--casadiApplyUnary ASinh  = C.asinh
--casadiApplyUnary ATanh  = C.atanh
--casadiApplyUnary ACosh  = C.acosh

casadiApplyBinary :: BinOp -> CasadiModule -> SX -> SX -> IO SX
casadiApplyBinary Add = C.add
casadiApplyBinary Sub = C.sub
casadiApplyBinary Mul = C.mul
casadiApplyBinary Div = C.div
casadiApplyBinary Pow = C.pow

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
showBinary Pow = "**"

paren :: Show a => a -> String
paren x = "( "++show x++" )"
