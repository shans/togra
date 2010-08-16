import SP
import SPUtil
import MSP
import Path
import TograStream
import Graphics.Rendering.OpenGL
import Timing
import Togra

theArr = ((aThenbThenc [[
    [Vertex3 (-2) (-2) 0, Vertex3 (-1) (-2) 1, 
	      Vertex3 1 (-2) (-1), Vertex3 2 (-2) 0],
    [Vertex3 (-2) 0 1, Vertex3 (-1) 0 0, Vertex3 1 0 2, Vertex3 2 0 1],
    [Vertex3 (-2) 2 0, Vertex3 (-1) 2 (-1), Vertex3 1 2 (-1), Vertex3 2 2 0]
		  ]]
		  [fromIntegral x / fromIntegral 40 | x <- [0..40]]
		  [fromIntegral x / fromIntegral 40 | x <- [0..40]])
     >>> bezierPatch >>> Batch 41 >>> Batch 41 >>> Arr (pairwiseL toQuads) 
     >>> concatMA >>> Arr (\a -> (a,a)))

main = do
	putStrLn (show theArr)
        togra 640 480 (tograMInT 5 theArr Quads)
