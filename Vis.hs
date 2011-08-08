-- Vis.hs

{-# OPTIONS_GHC -Wall #-}

module Vis(vis, VisShape, drawShapes) where

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Data.Time.Clock
import System.Posix.Unistd(usleep)

type VisShape a = (Object, (a,a,a), (a,a,a,a), (GLfloat,GLfloat,GLfloat))

data Camera = Camera { phi :: IORef GLdouble,
                       theta :: IORef GLdouble,
                       rho :: IORef GLdouble,
                       x0 :: IORef GLdouble,
                       y0 :: IORef GLdouble,
                       z0 :: IORef GLdouble,
                       ballX :: IORef GLint,
                       ballY :: IORef GLint, 
                       leftButton :: IORef GLint,
                       rightButton :: IORef GLint
                     }

makeCamera :: IO Camera
makeCamera = do
  phi'   <- newIORef 30
  theta' <- newIORef (20)
  rho'   <- newIORef 5
  x0'    <- newIORef 0
  y0'    <- newIORef 0
  z0'    <- newIORef 0
  ballX'  <- newIORef (-1)
  ballY'  <- newIORef (-1)
  leftButton' <- newIORef 0
  rightButton' <- newIORef 0
  return $ Camera { phi = phi',
                    theta = theta',
                    rho = rho',
                    x0 = x0',
                    y0 = y0',
                    z0 = z0',
                    ballX = ballX',
                    ballY = ballY',
                    leftButton = leftButton',
                    rightButton = rightButton'
                  }

myInit :: String -> IO ()
myInit progName = do
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 600
  _ <- createWindow progName

  clearColor $= Color4 0 0 0 0
  shadeModel $= Smooth
  depthFunc $= Just Less
  lighting $= Enabled
  light (Light 0) $= Enabled
  ambient (Light 0) $= Color4 1 1 1 1
   
  materialDiffuse Front $= Color4 0.5 0.5 0.5 1
  materialSpecular Front $= Color4 1 1 1 1
  materialShininess Front $= 25
  colorMaterial $= Just (Front, Diffuse)

drawAxes :: GLdouble -> GLdouble -> IO ()
drawAxes size aspectRatio = do
  let numSlices = 8
      numStacks = 15
      cylinderRadius = 0.5*size/aspectRatio
      cylinderHeight = size
      coneRadius = 2*cylinderRadius
      coneHeight = 2*coneRadius
      
      drawAxis r g b = do
        -- color
        materialDiffuse Front $= Color4 r g b 1
        color (Color3 r g b :: Color3 GLfloat)
        -- cylinder
        renderObject Solid (Cylinder' cylinderRadius cylinderHeight numSlices numStacks)
        -- cone
        translate (Vector3 0 0 cylinderHeight :: Vector3 GLdouble)
        renderObject Solid (Cone coneRadius coneHeight numSlices numStacks)
        
  -- red x axis
  preservingMatrix $ do
    rotate (90 :: GLdouble) (Vector3 0 1 0)
    drawAxis 1 0 0
    
  -- green y axis
  preservingMatrix $ do
    rotate (-90 :: GLdouble) (Vector3 1 0 0)
    drawAxis 0 1 0
  
  -- blue z axis
  preservingMatrix $ do
    drawAxis 0 0 1


drawShapes :: [VisShape GLdouble] -> IO ()
drawShapes shapes = do
  mapM_ drawShape shapes
    where
      drawShape :: VisShape GLdouble -> IO ()
      drawShape (cyl@(Cylinder' _ len _ _), (x,y,z), (q0,q1,q2,q3), (r,g,b)) = do
          preservingMatrix $ do
            materialDiffuse Front $= Color4 r g b 1
            color (Color3 r g b :: Color3 GLfloat)
            translate (Vector3 x y z :: Vector3 GLdouble)
            rotate (2*acos(q0)*180/pi :: GLdouble) (Vector3 q1 q2 q3)
            translate (Vector3 0 0 (-len/2) :: Vector3 GLdouble)
            renderObject Solid cyl
      drawShape (object, (x,y,z), (q0,q1,q2,q3), (r,g,b)) = do
          preservingMatrix $ do
            materialDiffuse Front $= Color4 r g b 1
            color (Color3 r g b :: Color3 GLfloat)
            translate (Vector3 x y z :: Vector3 GLdouble)
            rotate (2*acos(q0)*180/pi :: GLdouble) (Vector3 q1 q2 q3)
            renderObject Solid object


display :: IORef a -> Camera -> (a -> IO ()) -> DisplayCallback
display stateRef camera userDrawFun = do
   clear [ ColorBuffer, DepthBuffer ]
   
   -- draw the scene
   preservingMatrix $ do
     -- setup the camera
     x0'    <- get (x0    camera)
     y0'    <- get (y0    camera)
     z0'    <- get (z0    camera)
     phi'   <- get (phi   camera)
     theta' <- get (theta camera)
     rho'   <- get (rho   camera)
     let
       xc = x0' + rho'*cos(phi'*pi/180)*cos(theta'*pi/180)
       yc = y0' + rho'*sin(phi'*pi/180)*cos(theta'*pi/180)
       zc = z0' - rho'*sin(theta'*pi/180)
     lookAt (Vertex3 xc yc zc) (Vertex3 x0' y0' z0') (Vector3 0 0 (-1))
     
     -- draw ned axes
     drawAxes 0.5 5
     
     -- call user function
     state <- get stateRef
     userDrawFun state

     ---- draw the torus
     --color (Color3 0 1 1 :: Color3 GLfloat)
     --renderObject Solid (Torus 0.275 1.85 8 15)
   
   flush
   swapBuffers


reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 40 (fromIntegral w / fromIntegral h) 1 2000
   matrixMode $= Modelview 0
   loadIdentity
   postRedisplay Nothing


keyboardMouse :: Camera -> KeyboardMouseCallback
keyboardMouse camera key keyState _ _ = do
  case (key, keyState) of
    (Char '\27', Down) -> exitWith ExitSuccess

    (SpecialKey KeyLeft, Down)  -> print "left"
    (SpecialKey KeyRight, Down) -> print "right"
    (SpecialKey KeyUp, Down)    -> print "up"
    (SpecialKey KeyDown, Down)  -> print "down"
    
    (MouseButton LeftButton, Down) -> do 
      resetMotion
      leftButton camera $= 1
    (MouseButton LeftButton, Up) -> do 
      leftButton camera $= 0
    (MouseButton RightButton, Down) -> do 
      resetMotion
      rightButton camera $= 1
    (MouseButton RightButton, Up) -> do 
      rightButton camera $= 0
      
    (MouseButton WheelUp, Down) -> do zoom 0.9
    (MouseButton WheelDown, Down) -> do zoom 1.1
    
    _ -> return ()
    where resetMotion = do
            ballX camera $= -1
            ballY camera $= -1

          zoom factor = do
            rho camera $~ (* factor)
            postRedisplay Nothing
            

motion :: Camera -> MotionCallback
motion camera (Position x y) = do
   x0' <- get (x0 camera)
   y0' <- get (y0 camera)
   bx <- get (ballX camera)
   by <- get (ballY camera)
   phi' <- get (phi camera)
   theta' <- get (theta camera)
   rho' <- get (rho camera)
   lb <- get (leftButton camera)
   rb <- get (rightButton camera)
   let deltaX
         | bx == -1  = 0
         | otherwise = fromIntegral (x - bx)
       deltaY
         | by == -1  = 0
         | otherwise = fromIntegral (y - by)
       nextTheta 
         | deltaY + theta' >  80 =  80
         | deltaY + theta' < -80 = -80
         | otherwise             = deltaY + theta'
       nextX0 = x0' + 0.003*rho'*( -sin(phi'*pi/180)*deltaX - cos(phi'*pi/180)*deltaY)
       nextY0 = y0' + 0.003*rho'*(  cos(phi'*pi/180)*deltaX - sin(phi'*pi/180)*deltaY)
       
   if (lb == 1)
     then do phi   camera $~ (+ deltaX)
             theta camera $= nextTheta
     else do return ()
   if (rb == 1)
     then do x0 camera $= nextX0
             y0 camera $= nextY0
     else do return ()
   
   ballX camera $= x
   ballY camera $= y
   
   postRedisplay Nothing


vis :: (a -> a) -> (a -> IO ()) -> a -> Double -> IO ()
vis userSimFun userDrawFun x0' ts = do
  -- init glut/scene
  (progName, _args) <- getArgsAndInitialize
  myInit progName
   
  -- create internal state
  state <- newIORef x0'
  camera <- makeCamera
  t0 <- getCurrentTime
  lastTime <- newIORef t0

  -- setup callbacks
  displayCallback $= display state camera userDrawFun
  idleCallback $= Just (simCallback state userSimFun ts lastTime)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse camera)
  motionCallback $= Just (motion camera)
                 
  -- start drawing loop
  mainLoop
  

simCallback :: IORef a -> (a -> a) -> Double -> IORef UTCTime-> IO ()
simCallback stateRef userSimFun ts lastTimeRef = do
  currentTime <- getCurrentTime
  lastTime <- get lastTimeRef
  let usRemaining :: Int
      usRemaining = round $ 1e6*(ts - (realToFrac (diffUTCTime currentTime lastTime)))
    
  if usRemaining <= 0
    then do
      state <- get stateRef
      let nextState = userSimFun state
      stateRef $= nextState
      postRedisplay Nothing
      lastTimeRef $= addUTCTime (realToFrac ts) lastTime
    else do
      usleep usRemaining
