from togra import Togra, RenderFrame
from shapes import CircleGenerator
from transforms import Rotation
from streams import ConstantElement
from util import TimeOffset

from OpenGL.GL import *

rotation = Rotation()
rotation.connectInput(ConstantElement([1.0, 0.0, 0.0]), 0)
rotation.connectInput(TimeOffset(), 1)

togra = Togra()
togra.setRenderTree(RenderFrame(CircleGenerator(50), GL_LINE_LOOP, rotation))
togra.run()
