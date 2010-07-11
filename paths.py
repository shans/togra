from streams import ProcessingElement
from groups import Group

import math

def factFrom(f,t):
  tot = 1
  for i in range(f+1, t+1):
    tot *= i
  return tot

def binomials(size):
  scales = []
  calcTo = (size + 1) / 2
  for i in range(calcTo):
    scales.append(factFrom(i, (size - 1)) / factFrom(0, (size - i - 1)))
  for i in range(size - calcTo):
    scales.append(scales[size - calcTo - i - 1])
  return scales


# 
# v = SUM(i=0..n)(t^i (1-t)^(n-i) s_i p_i)
#
# v' = SUM(i=0..n)((it^(i-1) (1-t)^(n-i) - (t^i (n-i)(1-t)^(n-i-1)) s_i p_i
#
# e.g. v = t^2 p_0 + 2 t (1 - t) p_1 + (1 - t)^2 p_2
#      v' = 2 t p_0 + 2 (1 - t) p_1 - 2 t p_1 - 2 (1 - t) p_2

class Bezier(ProcessingElement):
  def __init__(this, numPoints):
    ProcessingElement.__init__(this, 1)
    this.numPoints = numPoints
  def run(this, points):
    points = points.getNext().values
    size = len(points)
    scales = binomials(size)
    out = Group()
    for i in range(this.numPoints):
      t = i / (this.numPoints - 1.0)
      tt = 1 - t
      pos = [0, 0, 0]
      tan = [0, 0, 0]
      for j in range(size):
        top = size - j - 1
        scale = scales[j] * math.pow(t, j) * math.pow(tt, top)
        tanScale = 0
        if j > 0:
          tanScale += j * math.pow(t, j - 1) * math.pow(tt, top)
        if j < size - 1:
          tanScale += (size - j - 1) * math.pow(t, j) * math.pow(tt, top - 1)
        tanScale *= scales[j]
        pos = [pos[0] + scale * points[j][0], pos[1] + scale * points[j][1],
               pos[2] + scale * points[j][2]]
        tan = [tan[0] + tanScale * points[j][0], 
               tan[1] + tanScale * points[j][1],
               tan[2] + tanScale * points[j][2]]
      out.addVector(pos)
      out.addTangent(tan)
    return out
