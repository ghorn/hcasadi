{-# OPTIONS_GHC -Wall #-}

module Casadi.Bindings( SX
                      , SXFunction
                      , casadiInit
                      , sym
                      , vsym
                      , msym
                      , sxFunction
                      , gradient
                      , jacobian
                      , hessian
                      , matrixMultiply
                      , transpose
                      , Casadi.Bindings.signum
                      , Casadi.Bindings.mul
                      , Casadi.Bindings.div
                      , Casadi.Bindings.add
                      , Casadi.Bindings.sub
                      , Casadi.Bindings.neg
                      , Casadi.Bindings.exp
                      , Casadi.Bindings.abs
                      , Casadi.Bindings.log
                      , Casadi.Bindings.acos
                      , Casadi.Bindings.asin
                      , Casadi.Bindings.atan
                      , Casadi.Bindings.cos
                      , Casadi.Bindings.cosh
                      , Casadi.Bindings.sin
                      , Casadi.Bindings.sinh
                      , Casadi.Bindings.tan
                      , Casadi.Bindings.tanh
                      , Casadi.Bindings.sqrt
                      , Casadi.Bindings.pow
                      ) where

import Python.Interpreter
import Python.Objects 
import System.IO.Unsafe(unsafePerformIO)

data SX = SX PyObject

data SXFunction = SXFunction PyObject

type CasadiModule = PyObject

instance Show SX where
--  show (SX x) = show x
  show = unsafePerformIO . pyShow
  
instance Show SXFunction where
--  show (SXFunction x) = show x
  show = unsafePerformIO . pyShow

instance FromPyObject SX where
  fromPyObject = return . SX

instance ToPyObject SX where
  toPyObject (SX x) = return x

instance FromPyObject SXFunction where
  fromPyObject = return . SXFunction

instance ToPyObject SXFunction where
  toPyObject (SXFunction x) = return x

--sxShowPyObject :: SX -> IO String
--sxShowPyObject (SX x) = showPyObject x
--
--sxReprOf :: SX -> IO String
--sxReprOf (SX x) = reprOf x

pyShow :: ToPyObject a => a -> IO String
pyShow x = callByNameHs "str" [x] noKwParms

-------------------------------------------------------------

sym :: CasadiModule -> String -> IO SX
sym casadi name = msym casadi name (1,1)
  
vsym :: CasadiModule -> String -> Int -> IO SX
vsym casadi name n = msym casadi name (n,1)
  
msym :: CasadiModule -> String -> (Int,Int) -> IO SX
--msym casadi name (r,c) = callMethodHs casadi "ssym" [toPyObject name, toPyObject r, toPyObject c] noKwParms
msym casadi name (r,c) = do
  ssym <- getattr casadi "ssym"
  name' <- toPyObject name
  r' <- toPyObject (toInteger r)
  c' <- toPyObject (toInteger c)
  mat <- pyObject_Call ssym [name', r', c'] []
  return (SX mat)
  
--------------------------------------------------------------
sxFunction :: CasadiModule -> [SX] -> [SX] -> IO SXFunction
sxFunction casadi xs zs = callMethodHs casadi "SXFunction" [xs, zs] noKwParms


-------------------------------------------------------------
gradient :: CasadiModule -> SX -> SX -> IO SX
gradient casadi ex args = callMethodHs casadi "gradient" [ex, args] noKwParms
  
jacobian :: CasadiModule -> SX -> SX -> IO SX
jacobian casadi ex args = callMethodHs casadi "jacobian" [ex, args] noKwParms

hessian :: CasadiModule -> SX -> SX -> IO SX
hessian casadi ex args = callMethodHs casadi "hessian" [ex, args] noKwParms

-------------------------------------------------------------
  
callSXMatrix :: String -> CasadiModule -> [SX] -> IO SX
callSXMatrix method casadi xs = do
  sxmatrix <- getattr casadi "SXMatrix"
  callMethodHs sxmatrix method xs noKwParms


binarySXMatrix :: String -> CasadiModule -> SX -> SX -> IO SX
binarySXMatrix method casadi x y = callSXMatrix method casadi [x,y]

unarySXMatrix :: String -> CasadiModule -> SX -> IO SX
unarySXMatrix method casadi x = callSXMatrix method casadi [x]

matrixMultiply :: CasadiModule -> SX -> SX -> IO SX
matrixMultiply = binarySXMatrix "mul"

signum :: CasadiModule -> SX -> SX -> IO SX
signum = binarySXMatrix "__sign__"

mul :: CasadiModule -> SX -> SX -> IO SX
mul = binarySXMatrix "__mul__"

div :: CasadiModule -> SX -> SX -> IO SX
div = binarySXMatrix "__div__"

add :: CasadiModule -> SX -> SX -> IO SX
add = binarySXMatrix "__add__"

sub :: CasadiModule -> SX -> SX -> IO SX
sub = binarySXMatrix "__sub__"

pow :: CasadiModule -> SX -> SX -> IO SX
pow = binarySXMatrix "__pow__"
-----------------------------------

transpose :: CasadiModule -> SX -> IO SX
transpose = unarySXMatrix "T"

neg :: CasadiModule -> SX -> IO SX
neg = unarySXMatrix "__neg__"

exp :: CasadiModule -> SX -> IO SX
exp = unarySXMatrix "exp"

abs :: CasadiModule -> SX -> IO SX
abs = unarySXMatrix "fabs"

log :: CasadiModule -> SX -> IO SX
log = unarySXMatrix "log"

acos :: CasadiModule -> SX -> IO SX
acos = unarySXMatrix "arccos"

asin :: CasadiModule -> SX -> IO SX
asin = unarySXMatrix "arcsin"

atan :: CasadiModule -> SX -> IO SX
atan = unarySXMatrix "arctan"

cos :: CasadiModule -> SX -> IO SX
cos = unarySXMatrix "cos"

cosh :: CasadiModule -> SX -> IO SX
cosh = unarySXMatrix "cosh"

sin :: CasadiModule -> SX -> IO SX
sin = unarySXMatrix "sin"

sinh :: CasadiModule -> SX -> IO SX
sinh = unarySXMatrix "sinh"

tan :: CasadiModule -> SX -> IO SX
tan = unarySXMatrix "tan"

tanh :: CasadiModule -> SX -> IO SX
tanh = unarySXMatrix "tanh"

sqrt :: CasadiModule -> SX -> IO SX
sqrt = unarySXMatrix "sqrt"

----------------------------------

casadiInit :: IO CasadiModule
casadiInit = do
  py_initialize
  pyImport_ImportModule "casadi"

--main :: IO ()
--main = do
--  casadi <- casadiInit
----  _ <- pyRun_SimpleString "from casadi import *"
----  print casadi
----  d <- pyModule_GetDict casadi
--  
----  b <- pyRun_SimpleString "casadi.ssym('t')"
----  print b
--  x <- vsym casadi "x" 5
--  y <- sym casadi "y"
--  
--  z <- mul casadi x y
--  z2 <- mul casadi x z
--  
--  fun <- sxFunction casadi [x,y] [z,z2]
--  
----  print z
----  print fun
--  
--  dz2dx <- hessian casadi z2 x
--  print dz2dx
--
----  z' <- sxShowPyObject z
----  z'' <- sxReprOf z
----  putStrLn z'
----  putStrLn z''
