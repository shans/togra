import SP
import SPUtil
import MSP
import Path
import TograStream
import Graphics.Rendering.OpenGL
import Timing
import Togra

divs = 40 

theArr = ((aThenbThenc [[
    [Vertex3 (-2) (-2) 0, Vertex3 (-1) (-2) 3, 
	      Vertex3 1 (-2) (-3), Vertex3 2 (-2) 0],
    [Vertex3 (-2) 0 (-3), Vertex3 (-1) 0 2, Vertex3 1 0 (-2), Vertex3 2 0 1],
    [Vertex3 (-2) 0 3, Vertex3 (-1) 0 2, Vertex3 1 0 (-2), Vertex3 2 0 1],
    [Vertex3 (-2) 2 0, Vertex3 (-1) 2 (-1), Vertex3 1 2 (-1), Vertex3 2 2 0]
		  ]]
		  [fromIntegral x / fromIntegral (divs-1) | x <- [0..(divs-1)]]
		  [fromIntegral x / fromIntegral (divs-1) | x <- [0..(divs-1)]])
     >>> bezierPatch >>> Batch divs >>> Batch divs >>> Arr (pairwiseL toQuads) 
     >>> concatMA >>> Arr (\a -> (a,a))) 
     >>> second (Unbatch >>> Batch 4 >>> quadNormal >>> repl 4 >>> Unbatch
			 >>> Batch ((divs - 1)*(divs - 1)*4))

main = do
	putStrLn (show theArr)
        togra 640 480 (tograMInT 5 theArr Quads)
