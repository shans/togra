import SP
import MSP
import TograStream
import Graphics.Rendering.OpenGL
import Togra

main = togra 640 480 (tograMIn (line 20 (Vertex3 0 0 0) (Vertex3 3 3 3)
		 >>> Batch 20 >>> Arr (\a -> (a,a))) LineStrip)
