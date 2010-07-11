from streams import ProcessingElement
from frames import Frame
from groups import Group

import math

class SquareGenerator(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 0)
  def run(this):
    out = Frame()
    out.addValue((0.5, 0.5, 0))
    out.addValue((-0.5, 0.5, 0))
    out.addValue((-0.5, -0.5, 0))
    out.addValue((0.5, -0.5, 0))
    return out

class CubeGenerator(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 0)
  def run(this):
    out = Group()
    out.addVector((0.5, 0.5, 0.5))
    out.addVector((-0.5, 0.5, 0.5))
    out.addVector((-0.5, -0.5, 0.5))
    out.addVector((0.5, -0.5, 0.5))
    out.addNormal((0.0, 0.0, 1.0))

    out.addVector((0.5, 0.5, -0.5))
    out.addVector((0.5, -0.5, -0.5))
    out.addVector((-0.5, -0.5, -0.5))
    out.addVector((-0.5, 0.5, -0.5))
    out.addNormal((0.0, 0.0, -1.0))
    
    out.addVector((-0.5, 0.5, -0.5))
    out.addVector((-0.5, -0.5, -0.5))
    out.addVector((-0.5, -0.5, 0.5))
    out.addVector((-0.5, 0.5, 0.5))
    out.addNormal((-1.0, 0.0, 0.0))

    out.addVector((0.5, 0.5, -0.5))
    out.addVector((0.5, 0.5, 0.5))
    out.addVector((0.5, -0.5, 0.5))
    out.addVector((0.5, -0.5, -0.5))
    out.addNormal((1.0, 0.0, 0.0))

    out.addVector((0.5, -0.5, -0.5))
    out.addVector((0.5, -0.5, 0.5))
    out.addVector((-0.5, -0.5, 0.5))
    out.addVector((-0.5, -0.5, -0.5))
    out.addNormal((0.0, -1.0, 0.0))

    out.addVector((0.5, 0.5, -0.5))
    out.addVector((-0.5, 0.5, -0.5))
    out.addVector((-0.5, 0.5, 0.5))
    out.addVector((0.5, 0.5, 0.5))
    out.addNormal((0.0, 1.0, 0.0))
    return out

class CircleGenerator(ProcessingElement):
  def __init__(this, numPoints):
    ProcessingElement.__init__(this, 0)
    this.numPoints = numPoints
  def run(this):
    out = Frame()
    for point in range(this.numPoints):
      angle = point * 2 * math.pi / this.numPoints
      out.addValue((math.cos(angle), math.sin(angle), 0))
    return out
    
class SphereLineGenerator(ProcessingElement):
  """
  Generate points that are distributed appropriately for slices of a sphere.
  We want points that are distributed evenly around a circumference, projected
  back onto the central axis.
  """
  def __init__(this, numPoints):
    ProcessingElement.__init__(this, 0)
    this.numPoints = numPoints
    this.point = 0
  def run(this):
    angle = this.point * math.pi / (this.numPoints - 1)
    this.point = (this.point + 1) % this.numPoints
    return (0, 0, math.cos(angle))

class SphereSliceSizeGenerator(ProcessingElement):
  def __init__(this, numPoints):
    ProcessingElement.__init__(this, 0)
    this.numPoints = numPoints
    this.point = 0
  def run(this):
    angle = this.point * math.pi / (this.numPoints - 1)
    this.point = (this.point + 1) % this.numPoints
    return math.sin(angle)

# TODO: consider replacing with a non-lifted version wrt shape, accompanied by a
# piecewise functor
class ScalingExtrusionElement(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 3)
  def run(this, point, scale, shape):
    out = Frame()
    scale = scale.getNext()
    point = point.getNext()
    for value in shape.getNext().values:
      out.addValue((value[0] * scale + point[1], value[1] * scale + point[1], value[2] * scale + point[2]))
    return out

class PointsToQuadsElement(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 1)
    this.lastFrame = None
  def run(this, frames):
    if this.lastFrame is None:
      this.lastFrame = frames.getNext()
    thisFrame = frames.getNext()
    out = Group()
    for i in range(min(len(thisFrame.values), len(this.lastFrame.values))) + [0]:
      out.addVector(this.lastFrame.values[i])
      out.addNormal(this.lastFrame.values[i])
      out.addVector(thisFrame.values[i])
      out.addNormal(thisFrame.values[i])

    this.lastFrame = thisFrame
    return out
