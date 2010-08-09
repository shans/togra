module Togra where

import Data.IORef
import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Shader
import SimpleShader
import SP
import TograUtil
import Vbo

data TograInput = DataStream ShaderTag DVBO
		| RenderPrimitive PrimitiveMode
		| End 

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

-- Going to have to fudge the size here for now.  Basically it should
-- be a property of the shader and the shadertag I think?
act :: TograShader -> IORef Int -> IORef (SP IO a TograInput) -> IO ()
act program size streamRef = do
  val <- getAndUpdate streamRef
  isEnd <- act' val
  fi isEnd (return ()) (act program size streamRef)
  return ()
    where
      act' (DataStream (Tag n t s) dvbo) = do
	setVarying program s dvbo
	size $= getVBOSize dvbo
	return False
      act' (RenderPrimitive mode) = do
	sizeVal <- get size
	drawArrays mode 0 (fromIntegral sizeVal)
	return False
      act' End = do
	return True

display program streamRef = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  translate (Vector3 0.0 0.0 (-6.0 :: GLfloat))
  size <- newIORef 0
  act program size streamRef
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
