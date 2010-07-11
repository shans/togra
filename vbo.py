from OpenGL.GL import *

class VBO:
  def __init__(this, type, data=None, dataType=None, renderType=GL_STATIC_DRAW):
    this.vbo = glGenBuffers(1)
    this.type = type
    if data is not None and dataType is not None:
      this.bufferData(data, dataType, renderType)
  def bind(this):
    glBindBuffer(this.type, this.vbo)
  def bufferData(this, data, dataType, renderType):
    this.bind()
    this.dataType = dataType
    glBufferData(this.type, data, renderType)
  def getDataType(this):
    return this.dataType
