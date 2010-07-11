# Ensure vertex array objects are available

def _initGLCompat():
  from OpenGL.GL.APPLE import vertex_array_object as v
  import OpenGL.arrays
  def _glGenVertexArrays(count):
    arr = OpenGL.arrays.GLuintArray.zeros(count)
    v.glGenVertexArraysAPPLE(count, arr)
    return arr

  globals()["glBindVertexArray"] = v.glBindVertexArrayAPPLE
  globals()["glGenVertexArrays"] = _glGenVertexArrays

def glBindVertexArray(arr):
  _initGLCompat()
  return glBindVertexArray(arr)

def glGenVertexArrays(count):
  _initGLCompat()
  return glGenVertexArrays(count)
