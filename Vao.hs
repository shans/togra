module Vao where

-- I'm going to have to fake these, at least for Macs, as there doesn't
-- seem to be support for the apple version of the vertex_array_object
-- extension.

-- argh argh argh hetlists strike again

import Foreign
import Graphics.Rendering.OpenGL
import Vbo


class Bindable a where
  bind :: a -> IO ()
  unbind :: a -> IO ()

data VAOEntry where
    E :: Storable a => 
      VBO -> AttribLocation -> VertexArrayDescriptor a -> VAOEntry

instance Bindable VAOEntry where
  bind (E vbo al vad) = do
    bindVBO vbo
    vertexAttribPointer al $= (ToFloat, vad)
    vertexAttribArray al $= Enabled
  unbind (E vbo al vad) = do
    vertexAttribArray al $= Disabled

data VAO = VAO [VAOEntry]

instance Bindable VAO where
  bind (VAO l) = mapM_ bind l
  unbind (VAO l) = mapM_ unbind l

emptyVAO :: VAO
emptyVAO = VAO []

addVBO :: Storable a => VAO -> VBO -> AttribLocation -> VertexArrayDescriptor a
		        -> VAO
addVBO (VAO l) vbo al vad = VAO ((E vbo al vad):l)

-- is it dangerous to use Ptr Float here?  Should I try and thread the type
-- through the DVBO?  That's probably not a bad idea anyway.
-- TODO: make DVBOs typed.
addDVBO :: DVBO -> AttribLocation -> VAO -> VAO
addDVBO (DVBO vbo dt s l) al vao = addVBO vao vbo al vad
  where
    vad = VertexArrayDescriptor (fromIntegral s) dt 0 (nullPtr :: Ptr Float)

