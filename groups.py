from streams import ProcessingCode, ProcessingElement

class GroupFinishedException:
  pass

class GroupElement:
  def __init__(this, code):
    this.code = code

VECTORS = GroupElement(0)
NORMALS = GroupElement(1)

class Group:
  def __init__(this):
    this.vectors = []
    this.normals = []
    this.tangents = []
  def addVector(this, vector):
    this.vectors.append(vector)
  def addNormal(this, normal):
    this.normals.append(normal)
  def addTangent(this, tangent):
    this.tangents.append(tangent)
  def add(this, value, element):
    if element == VECTORS:
      this.addVector(value)
    elif element == NORMALS:
      this.addNormal(value)
  class GroupAsOutput:
    def __init__(this, parent, element):
      this.vpos = -1
      this.npos = -1
      if element == VECTORS:
	this.iter = parent.vectors
      elif element == NORMALS:
	this.iter = parent.normals
      elif element == TANGENTS:
        this.iter = parent.tangents
    def getCurrent(this):
      if this.pos < 0:
	this. pos += 1
      if this.pos >= len(this.iter.values):
	raise GroupFinishedException()
      return this.iter.values[this.pos]
    def getNext(this):
      this.pos += 1
      if this.pos >= len(this.iter.values):
	raise GroupFinishedException()
      return this.iter.values[this.pos]
  def asOutput(this, element):
    return this.GroupAsOutput(this, element)

def groupProcessor(clazz, *elements):
  if len(elements) == 0:
    elements = [VECTORS, NORMALS, TANGENTS]
  class GroupProcessingElement(clazz):
    def __init__(this, *args):
      clazz.__init__(this, *args)
      this.extraInstances = []
      for i in range(len(elements) - 1):
	this.extraInstances.append(clazz(*args))
    def runOn(this, instance, element, inputs, result):
      inputs = map(lambda a : a.asOutput(element), inputs)
      while True:
	try:
	  result.add(instance.run(this, *inputs), element)
	except GroupFinishedException:
	  return
    def run(this, *inputs):
      first = elements[0]
      rest = elements[1:]
      inputs = map(lambda a : a.getNext(), inputs)
      result = Group()
      this.runOn(this, first, inputs, result)
      for i in range(len(rest)):
	this.runOn(this.extraInstances[i], rest[i], inputs, result)
      return result
  return GroupProcessingElement

