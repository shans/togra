import SP
import SPUtil
import MSP
import Path
import TograStream
import Graphics.Rendering.OpenGL
import Timing
import Togra

theArr = ((aThenb [[Vertex3 0 0 0, Vertex3 0 2 2, 
		   Vertex3 2 0 0, Vertex3 (-1) 1 1]]
		  [fromIntegral x / fromIntegral 20 | x <- [0..19]]) >>>
	   bezier >>> Batch 20 >>> Arr (\a -> (a,a)))

main = do
	putStrLn (show theArr)
        togra 640 480 (tograMInT 5 theArr LineStrip)
