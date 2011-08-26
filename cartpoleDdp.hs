-- cartpoleDdp.hs

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Hom
import Vis(drawShapes)
import Odes.Cartpole
import DdpCasadi(prepareDdp)

import qualified Data.Map as DM
import Data.Maybe (fromJust)
import Graphics.UI.GLUT

type ControllerState a = ([State a], [Action a])

-- cost fcn
cost :: Floating a => State a -> Action a -> a
cost state action = 10*x*x + x'*x' + 100*cos(theta) + theta'*theta' + 0.001*u*u + barrier
  where
    [x, x', theta, theta'] = state
    [u] = action

    -- barrier
    uUb =  10.1
    uLb = -10.1
    mu = 1.0
    uBarrierUb = -mu*log(  uUb - u )
    uBarrierLb = -mu*log( -uLb + u )
    barrier = uBarrierUb + uBarrierLb

drawFun :: (State Double, ControllerState Double) -> IO ()
drawFun (x, (xTraj, uTraj)) = do
  drawShapes (makeShapes x [0])
  let getOutput (x',u') key = realToFrac $ fromJust $ DM.lookup key (cartpoleOutputs x' u')
      getOutputs key = map (\x' -> getOutput x' key) $ tail $ zip xTraj uTraj
    
      bob_x = getOutputs "bob_x"
      bob_y = getOutputs "bob_y"

      bobPath :: [(GLfloat, GLfloat, GLfloat)]
      bobPath = zipWith (\a b -> (a,0,b)) bob_x bob_y
  
  preservingMatrix $ do
    materialDiffuse Front $= Color4 1 0.1 0.1 1
    color (Color3 1 0.1 0.1 :: Color3 GLfloat)
    renderPrimitive LineStrip $ mapM_ (\(a,b,c)->vertex$Vertex3 a b c) bobPath


-- discrete ode
dt :: Floating a => a
dt = 0.025

dode :: Floating a => State a -> Action a -> State a
dode x u = rk4Step cartpoleDxdt x u dt

-- run ddp
main :: IO ()
main = do let n = 70
              x0 = [-10,0,0.01,0::Double]
              u0 = [0::Double]

              xTrajBadGuess = replicate n x0
              uTrajBadGuess = replicate n u0

          ddp <- prepareDdp cost dode (4::Int) (1::Int) [(-10,10)]

          let (xTraj, uTraj, _) = head $ drop 50 $ ddp xTrajBadGuess uTrajBadGuess

              simController cddp x (xTrajPrev, uTrajPrev) = do
                let xTraj0 = x:(drop 2 xTrajPrev) ++ [last xTrajPrev]
                    uTraj0 = (tail uTrajPrev) ++ [last uTrajPrev]
                    (xTraj', uTraj', _) = head $ cddp xTraj0 uTraj0
                    u = head uTrajPrev
                return (u, (xTraj', uTraj'))

          cartpoleVis (simController ddp) drawFun (x0, (xTraj, uTraj)) dt
          print "hi"
