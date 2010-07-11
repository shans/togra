from togra import Togra, RenderFrame
from shapes import CubeGenerator
from transforms import Rotation
from streams import ConstantElement
from util import TimeOffset

from OpenGL.GL import *

rotation = Rotation()
rotation.connectInput(ConstantElement([1.0, 0.0, 0.0]), 0)
rotation.connectInput(TimeOffset(), 1)

togra = Togra()
togra.setRenderTree(RenderFrame(CubeGenerator(), GL_QUADS, rotation))
togra.run()
