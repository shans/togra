from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.arrays.lists import ListHandler
from OpenGL.GL.shaders import *
import OpenGL.arrays

import pygame
from pygame.locals import *

from gl_compat import *
from vbo import VBO
from vao import VAO

import numpy

pygame.init()
pygame.display.set_mode((200, 200), OPENGL | DOUBLEBUF)
glViewport(0, 0, 200, 200)
print glGetString(GL_VERSION)

VERTEX_SHADER = compileShader("""
#version 120

attribute vec3 in_Position;
attribute vec3 in_Color;
varying vec3 ex_Color;

void main(void)
{
  gl_Position = vec4(in_Position, 1.0);
  ex_Color = in_Color;
}
""", GL_VERTEX_SHADER)

FRAGMENT_SHADER = compileShader("""
#version 120

varying vec3 ex_Color;

void main(void)
{
  gl_FragColor = vec4(ex_Color, 1.0);
}
""", GL_FRAGMENT_SHADER)

PROGRAM = compileProgram(VERTEX_SHADER, FRAGMENT_SHADER)
print PROGRAM

glUseProgram(PROGRAM)

vert = numpy.array([-0.3, 0.5, -1.0, -0.8, -0.5, -1.0, 0.2, -0.5, -1.0], 'f')
vert2 = numpy.array([-0.2, 0.5, -1.0, 0.3, -0.5, -1.0, 0.8, 0.5, -1.0], 'f')
col = numpy.array([1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0], 'f')

glClearColor(1.0, 1.0, 1.0, 0.0)

vao1 = VAO()

vbo1 = VBO(GL_ARRAY_BUFFER, vert, GL_FLOAT)
vao1.associateVertexAttribVBO(0, vbo1, 3, 0)

vbo2 = VBO(GL_ARRAY_BUFFER, col, GL_FLOAT)
vao1.associateVertexAttribVBO(1, vbo2, 3, 0)

vao2 = VAO()

vbo3 = VBO(GL_ARRAY_BUFFER, vert2, GL_FLOAT)
vao2.associateVertexAttribVBO(0, vbo3, 3, 0)

vao2.unbind()

print "we here"

def run(draw):
  while 1:
    event = pygame.event.poll()
    if event.type == QUIT or \
        (event.type == KEYDOWN and event.key == K_ESCAPE):
      break
    draw()
    pygame.display.flip()

def draw():
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

  vao1.bind()
  glDrawArrays(GL_TRIANGLES, 0, 3)

  vao2.bind()
  glVertexAttrib3f(1, 1.0, 0.0, 0.0)
  glDrawArrays(GL_TRIANGLES, 0, 3)

  vao2.unbind()
  
  
run(draw)
