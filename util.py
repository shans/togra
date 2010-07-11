from streams import ProcessingElement
from frames import Frame
from groups import Group

import time

class TimeOffset(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 0)
    this.time = None
  def run(this):
    if this.time is None:
      this.time = time.time()
    return time.time() - this.time

class FrameAggregator(ProcessingElement):
  def __init__(this, numFrames):
    ProcessingElement.__init__(this, 1)
    this.numFrames = numFrames
  def run(this, frame):
    points = []
    for i in range(this.numFrames):
      points += frame.getNext().values
    return Frame(*points)

class GroupAggregator(ProcessingElement):
  def __init__(this, numGroups):
    ProcessingElement.__init__(this, 1)
    this.numGroups = numGroups
  def run(this, group):
    vectors = []
    normals = []
    for i in range(this.numGroups):
      vectors.extend(group.getNext().vectors)
      normals.extend(group.getCurrent().normals)
    out = Group()
    out.vectors = vectors
    out.normals = normals
    return out

class TakeFirst(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 1)
    this.value = None
  def run(this, inp):
    if this.value is None:
      this.value = inp.getNext()
    return this.value
