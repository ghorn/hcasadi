-- Main.hs

{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Wall -fno-cse -fno-full-laziness #-}
--{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Casadi.SXM


x = sym "x"


main = print x
