module Vbo where

import Foreign
import Graphics.Rendering.OpenGL

class Storable a => GlTypable a where
  glType :: a -> DataType
  multiplier :: a -> Int

instance GlTypable Double where
  glType _ = Double
  multiplier _ = 1

instance GlTypable Float where
  glType _ = Float
  multiplier _ = 1

instance GlTypable Int where
  glType _ = Int
  multiplier _ = 1

instance GlTypable a => GlTypable (Vertex3 a) where
  glType (Vertex3 a _ _) = glType a
  multiplier _ = 3

instance GlTypable a => GlTypable (Vertex4 a) where
  glType (Vertex4 a _ _ _) = glType a
  multiplier _ = 4

data VBO = VBO BufferObject BufferTarget BufferUsage

data DVBO = DVBO VBO DataType Int Int 

makeVBO :: BufferTarget -> BufferUsage -> IO VBO
makeVBO target usage = do
  vbo:[] <- (genObjectNames 1) :: IO [BufferObject]
  return (VBO vbo target usage)

bindVBO :: VBO -> IO ()
bindVBO (VBO vbo target usage) = do
  bindBuffer target $= Just vbo

putData :: (Show a, GlTypable a) => VBO -> [a] -> IO DVBO
putData vboObj [] = undefined 
putData vboObj l = do
  bindVBO vboObj
  let len = length l
  dataPtr <- mallocArray len
  pokeArray dataPtr l
  let (VBO vbo target usage) = vboObj
  bufferData target $= (fromIntegral (len * sizeOf (head l)), dataPtr, usage)
  let aData = head l
  return (DVBO vboObj (glType aData) (multiplier aData) len)

-- consider using mapBuffer and unmapBuffer here instead?
makeVBOWithData :: (Show a, GlTypable a) => BufferTarget -> BufferUsage -> [a] -> IO DVBO
makeVBOWithData target usage l = do
  vbo <- makeVBO target usage
  dvbo <- putData vbo l
  return dvbo

freeDataFromVBO :: DVBO -> IO ()
freeDataFromVBO (DVBO (VBO bo bt bu) dt _ _) = do
  (_, ptr, _) <- get $ bufferData bt
  free ptr

getVBOType :: DVBO -> DataType
getVBOType (DVBO _ dt _ _) = dt

getVBOSize :: DVBO -> Int
getVBOSize (DVBO _ _ _ s) = s
