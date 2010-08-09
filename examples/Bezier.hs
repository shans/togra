import SP
import SPUtil
import MSP
import Path
import TograStream
import Graphics.Rendering.OpenGL
import Timing
import Togra

-- TODO: work out why 3 3 3 is in a different place here and for the line 
-- example
main = togra 640 480 (tograMInT 5
	  (In [20] >>>
	   bezier' [Vertex3 0 0 0, Vertex3 0 2 2, 
                    Vertex3 2 0 0, Vertex3 (-1) 1 1]
	   >>> Arr (\a -> (a,a))) 
	  LineStrip)
