module Display (display, idle) where

import           Control.Monad
import           Cube
import           Data.IORef
import           Graphics.UI.GLUT

points :: [(GLfloat,GLfloat,GLfloat)]
points = [ (sin (2*pi*k/36), cos (2*pi*k/36), 0) | k <- [1..36] ]

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  (x', y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
      a <- get angle
      rotate a $ Vector3 0.3 0.2 1
      scale 0.7 0.7 (0.7::GLfloat)
      forM_ points $ \(x,y,z) ->
        preservingMatrix $ do
          color (Color3 ((1 + x ** 3) / 2) ((0.5 + y ** 3) / 2) ((1 + (x * y) ** 3) / 2) :: Color3 GLfloat)
          translate $ Vector3 x y z
          cube 0.05
          color $ Color3 (0::GLfloat) 0 0
          cubeFrame 0.05
  flush

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
    d <- get delta
    angle $~! (+ d)
    postRedisplay Nothing
