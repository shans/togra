from togra import Togra, RenderFrame
from streams import ProcessingElement, ConstantElement
from frames import Frame, frameProcessor, frameGenerator
from timing import timingProcessor, RatePrinter
from util import TimeOffset

import time
import math

from OpenGL.GL import *

class SinGenerator(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 2)
    this.offset = 0
    this.time = None
  def run(this, amplitude, frequency):
    if this.time is None:
      this.time = time.time()
    newTime = time.time()
    this.offset += (newTime - this.time) * 2 * math.pi * frequency.getNext()
    this.time = newTime
    return amplitude.getNext() * math.sin(this.offset)

class RectangleGenerator(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 2)
  def run(this, widthIn, heightIn):
    out = Frame()
    width = widthIn.getNext()
    height = heightIn.getNext()
    out.addValue((width/2.0, height/2.0, 0))
    out.addValue((width/2.0, -height/2.0, 0))
    out.addValue((-width/2.0, -height/2.0, 0))
    out.addValue((-width/2.0, height/2.0, 0))
    return out

togra = Togra()

rect = RectangleGenerator()
rect.connectInput(ConstantElement(4), 0)
sin = timingProcessor(SinGenerator, 1, "sin")()
sin.connectInput(TimeOffset(), 0)
sin.connectInput(TimeOffset(), 1)
rect.connectInput(sin, 1)

rectP = RatePrinter(1)
print rectP
rectP.connectInput(rect, 0)

togra.setRenderTree(RenderFrame(rectP, GL_QUADS))
togra.run()
