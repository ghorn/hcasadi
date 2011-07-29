-- spring.hs

--{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Ddp
import Data.List(mapAccumL, intersperse)
import Graphics.Gnuplot.Simple

-- spring ode
sDt :: Floating a => a
sDt = 0.025

sDxdt :: Floating a => Ode a
sDxdt state action = [v, (-k*x - b*v + u)/mass]
  where
    x    = state  !! 0
    v    = state  !! 1
    u    = action !! 0
    k    = 10
    b    = 0.8
    mass = 1

-- spring cost fcn
sCost :: Floating a => State a -> Action a -> a
sCost x u = (7*position*position + 2*velocity*velocity + 0.2*force*force)
  where
    position = x !! 0
    velocity = x !! 1
    force = u !! 0

-- euler step
sEuler :: Floating a => State a -> Action a -> a -> State a
sEuler x u dt = eulerStep sDxdt x u dt

-- rk4 step
sRk4 :: Floating a => State a -> Action a -> a -> State a
sRk4 x u dt = rk4Step sDxdt x u dt

-- dynamics linearizations dA/dB
-- f ~= dA*(x-x0) + dB*(u-u0) + f0
sA :: Floating a => State a -> Action a -> [[a]]
sA = dA sDxdt

sB :: Floating a => State a -> Action a -> [[a]]
sB = dB sDxdt

-- cost fcn quadratic expansion
sCx :: Floating a => State a -> Action a -> [a]
sCu :: Floating a => State a -> Action a -> [a]
sCxx :: Floating a => State a -> Action a -> [[a]]
sCuu :: Floating a => State a -> Action a -> [[a]]
sCxu :: Floating a => State a -> Action a -> [[a]]

sCx = cx sCost
sCu = cu sCost
sCxx = cxx sCost
sCuu = cuu sCost
sCxu = cxu sCost

-- q fcn quadratic expansion
sQ0  :: Floating a => State a -> Action a -> Quad a -> a
sQx  :: Floating a => State a -> Action a -> Quad a -> [a]
sQu  :: Floating a => State a -> Action a -> Quad a -> [a]
sQxx :: Floating a => State a -> Action a -> Quad a -> [[a]]
sQuu :: Floating a => State a -> Action a -> Quad a -> [[a]]
sQxu :: Floating a => State a -> Action a -> Quad a -> [[a]]

sQ0  = q0  sCost (\x u -> sRk4 x u sDt)
sQx  = qx  sCost (\x u -> sRk4 x u sDt)
sQu  = qu  sCost (\x u -> sRk4 x u sDt)
sQxx = qxx sCost (\x u -> sRk4 x u sDt)
sQuu = quu sCost (\x u -> sRk4 x u sDt)
sQxu = qxu sCost (\x u -> sRk4 x u sDt)


-- ddp
bp :: State Double -> Action Double -> Quad Double -> (Quad Double, [[Double]], [Double])
bp = backPropagate sCost (\x u -> sEuler x u sDt)

main :: IO ()
main = do let n = 100
              x0 = [10,0]
              u0 = [0]
              time :: [Double]
              time = take n [0,sDt..]
              
              xTraj0 = replicate n x0
              uTraj0 = replicate n u0
              
              v0 = Quad (sCxx x0 u0) (sCx x0 u0) (sCost x0 u0) x0
              
              bs :: [(Quad Double, [[Double]], [Double])]

              bs = foldr bpSweep [] (zip xTraj0 uTraj0)
              
              fs :: [(State Double, Action Double)]
              (_,fs) = mapAccumL fSweep x0 bs
              (xTraj, uTraj) = unzip fs

              pos = map (!! 0) xTraj
              vel = map (!! 1) xTraj
              force = map (!! 0) uTraj
          
          plotLists [] [zip time pos, zip time vel, zip time force]
          print $ "total cost: " ++ (show (sum (map (\(x,u) -> sCost x u) fs)))
                                     

bpSweep :: (State Double, Action Double) -> [(Quad Double, [[Double]], [Double])] -> [(Quad Double, [[Double]], [Double])]
bpSweep (x,u) [] = [bp x u (Quad vxx vx v0 x0)] -- end step - Vnext is 0 so q fcn is cost function
  where
    x0 = vx
    vxx = replicate (length x) vx
    vx = replicate (length x) v0
    v0 = 0
bpSweep (x,u) acc@((v,_,_):_) = (bp x u v):acc


fSweep :: Floating a => State a -> (Quad a, [[a]], [a]) -> (State a, (State a, Action a))
fSweep x (Quad _ _ _ x0, feedbackGain, uOpenLoop) = (xNext, (x, u))
  where
    u = controller x x0 feedbackGain uOpenLoop
    xNext = sEuler x u sDt

controller :: Floating a => State a -> State a -> [[a]] -> Action a -> Action a
controller x x0 feedbackMatrix uOpenLoop = u
  where
    u = zipWith (+) uOpenLoop uClosedLoop
    uClosedLoop = map (\a -> dotLists a deltaX) feedbackMatrix
      where
        dotLists xa xb = sum (zipWith (*) xa xb)
        deltaX = zipWith (-) x x0
