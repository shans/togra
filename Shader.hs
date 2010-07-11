module Shader (prepareShaders, setUniform, setVarying, bind, unbind,
               ShaderTag(Tag), getTagType, shaderTags) where

import Data.ObjectName
import Data.StateVar
import Data.IORef
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Monad
import TograUtil
import Vao
import Vbo

instance Show (IORef a) where
  show a = "an IORef"

data TograShader = TograShader Program (IORef VAO)
  deriving (Eq, Show)

instance Bindable a => Bindable (IORef a) where
  bind v = do
    vo <- get v
    bind vo
  unbind v = do
    vo <- get v
    unbind vo

instance Bindable TograShader where
  bind (TograShader program vao) = do
    currentProgram $= Just program
    bind vao
  unbind (TograShader program vao) = do
    currentProgram $= Nothing
    unbind vao

compileAndCheckShader :: Shader s => s -> IO ()
compileAndCheckShader s = do
  compileShader s
  result <- get (compileStatus s)
  check result (get (shaderInfoLog s))

prepareShaders :: [String] -> [String] -> IO TograShader
prepareShaders vertexShaderProgram fragmentShaderProgram = do
  (vertexShader):[] <- (genObjectNames 1) :: IO [VertexShader]
  (shaderSource vertexShader) $= vertexShaderProgram
  compileAndCheckShader vertexShader  
  (fragmentShader):[] <- (genObjectNames 1) :: IO [FragmentShader]
  (shaderSource fragmentShader) $= fragmentShaderProgram
  compileAndCheckShader fragmentShader
  (program):[] <- (genObjectNames 1) :: IO [Program]
  (attachedShaders program) $= ([vertexShader], [fragmentShader])
  linkProgram program
  result <- get (linkStatus program)
  check result (get (programInfoLog program))
  validateProgram program
  result <- get (validateStatus program)
  check result (get (programInfoLog program))
  vao <- newIORef emptyVAO
  return (TograShader program vao)

setUniform :: Uniform a => TograShader -> String -> a -> IO ()
setUniform (TograShader program vao) name value = do
  loc <- get (uniformLocation program name)
  uniform loc $= value
  putStrLn name
  checkGlErrors

--setVarying :: VertexAttrib a => Program -> String -> Ptr a -> IO ()
--  loc <- get (attribLocation program name)
--  vertexAttribv ToFloat loc value

setVarying :: TograShader -> String -> DVBO -> IO ()
setVarying (TograShader program vao) name dvbo = do
  loc <- get (attribLocation program name)
  vao $~ addDVBO dvbo loc
  -- ensure that if this program is bound then the newly set varying value
  -- is bound too
  current <- get currentProgram
  if current == Just program then bind vao else return ()
  putStrLn name
  checkGlErrors

data ShaderTag = Tag GLint VariableType String

getTagType :: ShaderTag -> VariableType
getTagType (Tag _ vt _) = vt

shaderTags :: TograShader -> IO [ShaderTag]
shaderTags (TograShader p vao) = do
  l <- get $ activeAttribs p
  return $ map (\(n,v,s) -> Tag n v s) l
