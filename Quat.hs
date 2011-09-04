-- Quat.hs

module Quat( Quat(..)
          ) where

data Quat a = Quat a a a a deriving (Show, Eq)
