import time
from streams import ProcessingElement

class RatePrinter(ProcessingElement):
  def __init__(this, interval):
    ProcessingElement.__init__(this, 1)
    this.start = None
    this.count = 0
    this.interval = interval
  def run(this, input):
    now = time.time()
    if this.start is None:
      this.start = now
    if now - this.start > this.interval:
      this.start += this.interval
      print (1.0*this.count/this.interval),"fps"
      this.count = 0
    this.count += 1
    return input.getNext()

# convert a ProcessingElement into one that collects statistics
def timingProcessor(clazz, interval, name):
  class TimingProcessingElement(clazz):
    def __init__(this, *args):
      clazz.__init__(this, *args)
      this.intervalStart = None
    def run(this, *inputProcs):
      before = time.time()
      result = clazz.run(this, *inputProcs)
      after = time.time()
      if this.intervalStart is None:
	this.intervalStart = before
	this.results = []
      this.results.append(after - before)
      if (after - this.intervalStart) > interval:
	maxCost = max(this.results)
	minCost = min(this.results)
	average = 0.0
	for tresult in this.results:
	  average += tresult
	average /= len(this.results)
	deviation = 0.0
	for tresult in this.results:
	  deviation += (tresult - average) * (tresult - average)
	deviation /= len(this.results)
	print "%s: min=%f max=%f av=%f (persec=%f) sd=%f" % \
	    (name, minCost, maxCost, average, 1/average, deviation)
	this.intervalStart = None
      return result
  return TimingProcessingElement
