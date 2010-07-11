from OpenGL.GL import *
from OpenGL.GLU import *
import pygame
from pygame.locals import *
from OpenGL.arrays.lists import ListHandler
from vbo import VBO
from vao import VAO
from numpy import array
import simple_shader

class TograWindow_PyGame:
  def init(this, width, height):
    pygame.init()
    pygame.display.set_mode((width, height), OPENGL | DOUBLEBUF)

  def run(this, handler):
    while 1:
      event = pygame.event.poll()
      if event.type == QUIT or \
	  (event.type == KEYDOWN and event.key == K_ESCAPE):
	break
      handler.draw()
      pygame.display.flip()
      
class RenderFrame:
  def __init__(this, stream, renderType, transformStream = None):
    this.stream = stream.registerOutput()
    this.renderType = renderType
    this.shader = None
    if transformStream is not None:
      this.transformStream = transformStream.registerOutput()
    else:
      this.transformStream = None
  def flatten(this, list):
    newList = []
    for (a,b,c) in list:
      newList += [a,b,c]
    return newList
  def renderV(this, vectors):
    for vector in vectors:
      glVertex3f(vector[0], vector[1], vector[2])
  def renderVN(this, vectors, normals, vectorsPerNormal):
    if this.shader is None:
      this.len = len(vectors)
      vectors = array(this.flatten(vectors), 'f')
      normals = array(this.flatten(normals), 'f')
      ###
      vectors = array([-1.0, -1.0, 0.0, -1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, -1.0, 0.0], 'f')
      normals = vectors
      this.renderType = GL_QUADS
      this.len = 4
      ###
      vbo1 = VBO(GL_ARRAY_BUFFER, vectors, GL_FLOAT)
      vbo2 = VBO(GL_ARRAY_BUFFER, normals, GL_FLOAT)
      shader = simple_shader.Shader()
      shader.setVertexPosition(vbo1, 3, 0)
      shader.setVertexNormal(vbo2, 3, 0)
      shader.use()
      shader.setLights(0, "location", (6.0, 2.0, 4.0))
      shader.setLights(0, "diffuse", (1.0, 1.0, 1.0, 1.0))
      shader.setLights(0, "ambient", (0.2, 0.2, 0.2, 1.0))
      shader.setLights(0, "specular", (0.3, 1.0, 0.3, 1.0))
      shader.setLights(1, "location", (-6.0, -2.0, 4.0))
      shader.setLights(1, "diffuse", (1.0, 1.0, 1.0, 1.0))
      shader.setLights(1, "ambient", (0.2, 0.2, 0.2, 1.0))
      shader.setLights(1, "specular", (1.0, 0.3, 0.3, 1.0))
      
      shader.setGlobalAmbient((0.3, 0.05, 0.05, 0.1))
      
      shader.setMaterial("diffuse", (0.5, 0.5, 0.5, 1.0))
      shader.setMaterial("ambient", (0.2, 0.2, 0.2, 1.0))
      shader.setMaterial("shininess", 10.0)
      shader.setMaterial("specular", (1.0, 1.0, 1.0, 1.0))
      this.shader = shader

    this.shader.bind()
    glDrawArrays(this.renderType, 0, this.len)
    this.shader.unbind()

  def render(this):
    if not this.transformStream is None:
      glMultMatrixd(this.transformStream.getNext())
    frame = this.stream.getNext()
    # render a frame
    if (frame.__dict__.has_key("values")):
      glBegin(this.renderType)
      this.renderV(frame.values)
      glEnd()
    # render a group
    elif (frame.__dict__.has_key("vectors")):
      vectors = frame.vectors
      normals = frame.normals
      if len(normals) is 0:
	glBegin(this.renderType)
	this.renderV(vectors)
	glEnd()
      else:
	this.renderVN(vectors, normals, len(vectors) / len(normals))

class Togra:
  def __init__(this, windowClass=TograWindow_PyGame, width=640, height=480):
    this.width = width
    this.height = height
    this.window = windowClass()
    this.window.init(width, height)
    this.resize(width, height)
    this.init()
  
  def resize(this, width, height):
    glViewport(0, 0, width, height)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    gluPerspective(45, 1.0*width/height, 0.1, 100.0)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()

  def init(this):
    glShadeModel(GL_SMOOTH)
    glClearColor(0.9, 0.9, 1.0, 1.0)
    glClearDepth(1.0)
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LEQUAL)
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST)

    #glLightfv(GL_LIGHT1, GL_AMBIENT, (0.5, 0.5, 0.5, 1.0))
    #glEnable(GL_LIGHT1)
    #glEnable(GL_LIGHTING)

    #glEnable(GL_CULL_FACE)

    #glMaterialfv(GL_FRONT, GL_SPECULAR, (1.0, 1.0, 1.0, 1.0))
    #glMaterialf(GL_FRONT, GL_SHININESS, 25.0)


  def draw(this):
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glLoadIdentity()
    glTranslatef(0.0, 0.0, -6.0)
    this.renderTree.render()

  def setRenderTree(this, tree):
    this.renderTree = tree

  def run(this):
    this.window.run(this)
