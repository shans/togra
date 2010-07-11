# Python-only implementation of streams.
class ProcessingElement:
  """
  A single element of a processing stream.  Extend these to form elements that
  do things (see for example the simple Elements below).
  """
  def __init__(this, numInputs):
    this.inputs = [None] * numInputs
  def getNext(this):
    result = this.run(*this.inputs)
    return result
  def connectInput(this, input, inputSlot):
    this.inputs[inputSlot] = input

# Some very simple examples
class AddOffsetElement(ProcessingElement):
  def __init__(this, offset):
    ProcessingElement.__init__(this, 1)
    this.offset = offset
  def run(this, value):
    return value.getNext() + this.offset

class AddTwoValuesElement(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 2)
  def run(this, a, b):
    return a.getNext() + b.getNext()

class ConstantElement(ProcessingElement):
  def __init__(this, constant):
    ProcessingElement.__init__(this, 0)
    this.constantValue = constant
  def run(this):
    return this.constantValue

class IncrementingElement(ProcessingElement):
  def __init__(this):
    ProcessingElement.__init__(this, 0)
    this.value = -1
  def run(this):
    this.value += 1
    return this.value

# Some tests!
if __name__ == "__main__":
  constant = ConstantElement(7)
  addOffset = AddOffsetElement(4)
  addOffset.connectInput(constant, 0)
  assert addOffset.getNext() == 11
  assert addOffset.getNext() == 11
  
  incr = IncrementingElement()
  assert incr.getNext() == 0
  assert incr.getNext() == 1
  assert incr.getNext() == 2

  constant = ConstantElement(6)
  incr = IncrementingElement()
  addTwo = AddTwoValuesElement()
  addTwo.connectInput(constant, 0)
  addTwo.connectInput(incr, 1)
  assert addTwo.getNext() == 6
  assert addTwo.getNext() == 7
