module Togra where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Shader
import SimpleShader
import SP
import TograUtil
import Vbo

data TograInput = DataStream ShaderTag DVBO   

togra :: GLsizei -> GLsizei -> ([ShaderTag] -> SP IO () TograInput) -> IO ()
togra w h stream = do 
  (progname, _) <- getArgsAndInitialize
  initialWindowSize $= Size w h 
  initialDisplayMode $= [ DoubleBuffered, WithDepthBuffer ]
  createWindow "Togra"
  shader <- prepareShaders vertexShaderProgram fragmentShaderProgram
  setShaderDefaults shader
  initTogra
  reshapeCallback $= Just reshape
  tags <- shaderTags shader
  streamRef <- newIORef $ stream tags
  displayCallback $= display shader streamRef
  mainLoop

initTogra = do
  -- probably don't need to do this
  depthMask $= Enabled
  depthFunc $= Just Lequal
  hint PerspectiveCorrection $= Nicest
  version <- get glVersion
  putStrLn version

reshape size = do
  viewport $= (Position 0 0, size)
  clearColor $= Color4 0.9 0.9 1.0 1.0
  clearDepth $= 1.0
  matrixMode $= Projection
  loadIdentity
  let Size w h = size
  perspective 45 ((fromIntegral w) / (fromIntegral h)) 0.1 100.0
  matrixMode $= Modelview 0
  loadIdentity
  putStrLn "reshaped"

process (Put v s') = return (v, s')
process (Block ms') = do
    s' <- ms'
    (v, s'') <- process s'
    return (v, s'')
 
getAndUpdate :: IORef (SP IO a b) -> IO b
getAndUpdate ref = do
  s <- get ref
  (v, s') <- process s
  ref $= s'
  return v

display program streamRef = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  translate (Vector3 0.0 0.0 (-6.0 :: GLfloat))
  -- how do we know how many?
  (DataStream (Tag n t s) dvbo) <- getAndUpdate streamRef
  setVarying program s dvbo
  (DataStream (Tag n t s) dvbo) <- getAndUpdate streamRef
  setVarying program s dvbo
  let l = getVBOSize dvbo
  drawArrays Quads 0 (fromIntegral l)
  checkGlErrors
  swapBuffers
  postRedisplay Nothing
  

vertex3 :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3 a b c = Vertex3 a b c

vertex4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Vertex4 GLfloat
vertex4 a b c d = Vertex4 a b c d

setShaderDefaults program = do
  bind program
  setUniform program "Lights[0].location" (vertex3 6.0 2.0 4.0)
  setUniform program "Lights[0].diffuse" (vertex4 1.0 1.0 1.0 1.0)
  setUniform program "Lights[0].ambient" (vertex4 0.2 0.2 0.2 1.0)
  setUniform program "Lights[0].specular" (vertex4 0.3 1.0 0.3 1.0)
  setUniform program "GlobalAmbient" (vertex4 0.3 0.05 0.05 0.1)
  setUniform program "Material.diffuse" (vertex4 0.5 0.5 0.5 1.0)
  setUniform program "Material.ambient" (vertex4 0.2 0.2 0.2 1.0)
  --setUniform program "Material.shininess" (10.0 :: GLfloat)
  setUniform program "Material.specular" (vertex4 1.0 1.0 1.0 1.0)
  --let d = [Vertex3 (-1.0 :: Float) (-1.0) 0.0, 
  --	   Vertex3 (-1.0) 1.0 0.0, 
  --	   Vertex3 1.0 1.0 0.0,
  --	   Vertex3 1.0 (-1.0) 0.0]
  --dvbo <- makeVBOWithData ArrayBuffer StaticDraw d 
  --setVarying program "VertexPosition" dvbo
  --setVarying program "VertexNormal" dvbo
  --return ()  
