import SP
import SPUtil
import MSP
import Path
import TograStream
import Graphics.Rendering.OpenGL
import Timing
import Togra

main = togra 640 480 (tograMInT 5
	  (In [0..19] >>> Arr (\a -> fromIntegral a / fromIntegral 20) >>>
	   bezier' [Vertex3 0 0 0, Vertex3 0 2 2, 
                    Vertex3 2 0 0, Vertex3 (-1) 1 1]
	   >>> Batch 20 id >>> Arr (\a -> (a,a))) 
	  LineStrip)
