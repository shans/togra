import SP
import MSP
import Path
import TograStream
import Graphics.Rendering.OpenGL
import Timing
import Togra

main = togra 640 480 (tograMInT 5
	  (line 20 (Vertex3 0 0 0) (Vertex3 3 3 3) >>> Batch 20 id >>> 
           Arr (\a -> (a,a))) 
	  LineStrip)
