from streams import ProcessingElement

import math

class Rotation(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 2)
  def run(this, axisIn, amountIn):
    axis = axisIn.getNext()
    ux = axis[0]
    uy = axis[1]
    uz = axis[2]
    amount = amountIn.getNext()
    c = math.cos(amount)
    s = math.sin(amount)
    ux2 = ux * ux
    uy2 = uy * uy
    uz2 = uz * uz
    uxy = ux * uy
    uxz = ux * uz
    uyz = uy * uz
    
    return \
      [ux2 + (1 - ux2) * c, uxy * (1 - c) + uz * s, uxz * (1 - c) - uy * s, 0,
       uxy * (1 - c) - uz * s, uy2 + (1 - uy2) * c, uyz * (1 - c) + ux * s, 0,
       uxz * (1 - c) + uy * s, uyz * (1 - c) - ux * s, uz2 + (1 - uz2) * c, 0,
       0, 0, 0, 1]
