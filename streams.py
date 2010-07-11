# Python-only implementation of streams.
#
# A Stream is a collection of ProcessingElements, each of which has a number of
# inputs and a single output.  ProcessingElements are connected together to
# form streams that process data.

class ProcessingCode:
  def __init__(this, code):
    this.code = code

NO_OUTPUT = ProcessingCode(0)
CALL_ME_AGAIN = ProcessingCode(1)

class StreamAlreadyStartedException:
  pass

class ProcessingElement:
  """
  A single element of a processing stream.  Extend these to form elements that
  do things (see for example the simple Elements below).

  Initialise your ProcessingElement with the desired number of inputs and
  provide a definition of run() with the correct number of input parameters.
  Make sure your run definition returns either a result, NO_OUTPUT (for no
  result), or CALL_ME_AGAIN (which will call run() again with the next values
  of the upstream streams).

  Define functional to be true if the output of this element is constant for
  constant inputs.
  """
  def __init__(this, numInputs, functional=False):
    this.inputs = [None] * numInputs
    this.outputs = []
    this.current = []
    this.started = False
    this.constant = False
  def getCurrent(this, lid):
    if this.current[lid] == -1:
      return this.getNext(lid)
    if this.outputs[this.current[lid]] is NO_OUTPUT:
      return None
    return this.outputs[this.current[lid]]
  def getNext(this, lid):
    # we call a function on this class called run.  We provide
    # each input as an argument, and we expect an output.  Never return
    # None!  This will be flagged as an error.  Instead use the values
    # defined above.
    this.current[lid] += 1
    if len(this.outputs) == this.current[lid]:
      result = CALL_ME_AGAIN
      while result is CALL_ME_AGAIN:
        result = this.run(*this.inputs)
      this.outputs.append(result)
    else:
      result = this.outputs[this.current[lid]]
    minOutput = min(this.current)
    if minOutput > 0:
      this.outputs = this.outputs[minOutput:]
      this.started = True
      this.current = map(lambda a: a - minOutput, this.current)
    return result
  def connectInput(this, input, inputSlot):
    if input.started:
      raise StreamAlreadyStartedException()
    this.inputs[inputSlot] = input.registerOutput()

  class ProcessingElementAsConstantOutput:
    def __init__(this, value):
      this.value = value
    def getCurrent(this):
      return this.value
    def getNext(this):
      return this.value
  
  class ProcessingElementAsOutput:
    def __init__(this, actualElement, lid):
      this.actualElement = actualElement
      this.lid = lid
    def getCurrent(this):
      "get the current value from this output.  Do not call this without first calling getNext"
      return this.actualElement.getCurrent(this.lid)
    def getNext(this):
      "get the next value from this output"
      return this.actualElement.getNext(this.lid)

  def registerOutput(this):
    """
    stream.registerOutput() returns an iterator with getCurrent() and getNext()
    methods.  Each registered output behaves independently of the others.
    """
    if this.constant:
      if this.value is None:
        this.value = ProcessingElementAsOutput(this, 0).getNext()
      return ProcessingElementAsConstantOutput(this.value)
    
    lid = len(this.current)
    this.current.append(-1)
    return this.ProcessingElementAsOutput(this, lid)

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
  iter = addOffset.registerOutput()
  assert iter.getNext() == 11
  assert iter.getNext() == 11
  
  incr = IncrementingElement()
  iter1 = incr.registerOutput()
  iter2 = incr.registerOutput()
  assert iter1.getNext() == 0
  assert iter1.getNext() == 1
  assert iter2.getNext() == 0
  assert iter1.getNext() == 2
  assert iter2.getNext() == 1
