from togra import Togra, RenderFrame
from shapes import SphereLineGenerator, SphereSliceSizeGenerator, \
		   ScalingExtrusionElement, CircleGenerator, \
		   PointsToQuadsElement
from transforms import Rotation
from streams import ConstantElement
from util import TimeOffset, GroupAggregator, TakeFirst
from frames import frameGenerator

from timing import timingProcessor, RatePrinter

from OpenGL.GL import *

SLICES = 100
PPS = 100

rotation = Rotation()
rotation.connectInput(ConstantElement([1.0, 0.0, 0.0]), 0)
rotation.connectInput(TimeOffset(), 1)

genF = timingProcessor(ScalingExtrusionElement, 1, "extrusion")()
genF.connectInput(timingProcessor(SphereLineGenerator, 1, "slg")(SLICES), 0)
genF.connectInput(timingProcessor(SphereSliceSizeGenerator, 1, "sssg")(SLICES), 1)
genF.connectInput(timingProcessor(CircleGenerator, 1, "cg")(PPS), 2)

genQ = timingProcessor(PointsToQuadsElement, 1, "pointsToQuads")()
genQ.connectInput(genF, 0)

genG = timingProcessor(GroupAggregator, 1, "aggregator")(SLICES)
genG.connectInput(genQ, 0)

genC = TakeFirst()
genC.connectInput(genG, 0)

gen = RatePrinter(1)
gen.connectInput(genC, 0)

togra = Togra()
togra.setRenderTree(RenderFrame(gen, GL_QUAD_STRIP, rotation))
togra.run()
