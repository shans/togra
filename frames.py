# An attempt to implement output frames entirely on top of streams.
#
# A frame contains a set of values that should all be treated the same - for example,
# all of the vertices in a shape.
#
# This module contains the following functors:
# frameProcessor(ProcessingElement): convert a processingElement that operates on objects
# a, b, ... n to a processingElement that operates on frames of objects a, b, ... n
# frameGenerator(ProcessingElement, size): convert a processingElement that generates
# objects of type a to one that generates frames of objects of type a of the given size.


from streams import ProcessingCode, ProcessingElement

class FrameFinishedException:
  pass

class Frame:
  def __init__(this, *values):
    this.values = list(values)
  def addValue(this, value):
    this.values.append(value)
  class FrameAsOutput:
    def __init__(this, parent):
      this.pos = -1
      this.parent = parent
    def getCurrent(this):
      if this.pos < 0:
        this.pos += 1
      if this.pos >= len(this.parent.values):
        raise FrameFinishedException()
      return this.parent.values[this.pos]
    def getNext(this):
      this.pos += 1
      if this.pos >= len(this.parent.values):
        raise FrameFinishedException()
      return this.parent.values[this.pos]
  def asOutput(this):
    return this.FrameAsOutput(this)

# convert a standard ProcessingElement to one that deals with frames
def frameProcessor(clazz):
  class FrameProcessingElement(clazz):
    def run(this, *inputs):
      inputs = map(lambda a : a.getNext().asOutput(), inputs)
      result = Frame()
      while True:
        try:
          result.addValue(clazz.run(this, *inputs))
        except FrameFinishedException:
          return result
  return FrameProcessingElement

def frameGenerator(clazz, size):
  class FrameGeneratingProcessingElement(clazz):
    def run(this, *inputs):
      result = Frame()
      for val in range(size):
        result.addValue(clazz.run(this, *inputs))
      return result
  return FrameGeneratingProcessingElement

# Some tests
if __name__ == "__main__":
  from streams import AddOffsetElement, AddTwoValuesElement
  from streams import ConstantElement, IncrementingElement
  
  constant = frameGenerator(ConstantElement, 5)(4)
  addOffset = frameProcessor(AddOffsetElement)(12)
  addOffset.connectInput(constant, 0)
  incr = frameGenerator(IncrementingElement, 7)()
  adder = frameProcessor(AddTwoValuesElement)()
  adder.connectInput(addOffset, 0)
  adder.connectInput(incr, 1)

  iter = constant.registerOutput()
  assert iter.getNext().values == [4,4,4,4,4]
  assert iter.getNext().values == [4,4,4,4,4]

  iter = addOffset.registerOutput()
  assert iter.getNext().values == [16,16,16,16,16]
  assert iter.getNext().values == [16,16,16,16,16]

  iter = incr.registerOutput()
  assert iter.getNext().values == [0,1,2,3,4,5,6]
  assert iter.getNext().values == [7,8,9,10,11,12,13]

  iter = adder.registerOutput()
  assert iter.getNext().values == [16,17,18,19,20]
  assert iter.getNext().values == [23,24,25,26,27]

  print "OK"
