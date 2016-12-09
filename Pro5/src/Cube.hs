module Cube where

import           Graphics.UI.GLUT

import           Utility

cube :: GLfloat -> IO ()
cube w = let
    points = [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
               ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
               ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
               (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
               ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
               ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]
    in renderPrimitive Quads $ mapM_ (vertex . uncurry3 Vertex3) points

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ (vertex . uncurry3 Vertex3)
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]
