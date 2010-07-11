from togra import Togra, RenderFrame
from paths import Bezier
from transforms import Rotation
from streams import ProcessingElement, ConstantElement
from util import TimeOffset
from frames import Frame

from OpenGL.GL import *

rotation = Rotation()
rotation.connectInput(ConstantElement([1.0, 0.0, 0.0]), 0)
rotation.connectInput(TimeOffset(), 1)

class Points(ProcessingElement):
  def __init__(this, points):
    ProcessingElement.__init__(this, 0)
    this.points = points
  def run(this):
    out = Frame()
    out.values = this.points
    return out

bezier = Bezier(50)
bezier.connectInput(Points([[0.0, 0.0, 0.0], 
                            [1.0, 1.0, 1.0], 
                            [0.0, 0.0, 2.0],
                            [2.0, 0.0, 1.0]]), 0)

togra = Togra()
togra.setRenderTree(RenderFrame(bezier, GL_LINE_STRIP, rotation))
togra.run()

