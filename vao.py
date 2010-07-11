from gl_compat import *
from OpenGL.GL import *

class VAO:
  def __init__(this):
    this.vao = glGenVertexArrays(1)[0]
    this.vertexAttribVBOs = {}
  def bind(this):
    glBindVertexArray(this.vao)
  def associateVertexAttribVBO(this, index, vbo, size, stride):
    this.vertexAttribVBOs[index] = vbo
    this.bind()
    vbo.bind()
    glVertexAttribPointer(index, size, vbo.getDataType(), GL_FALSE, stride, None)
    glEnableVertexAttribArray(index)
  def unbind(this):
    glBindVertexArray(0)
