module Main where

import UI.Command
import Data.Default
import Control.Monad.Trans

import MSP
import SP
import Path
import Timing
import Togra
import SPUtil
import Graphics.Rendering.OpenGL

main = appMain examples

examples :: Application () ()

examples = def {
  appName = "Examples",
  appVersion = "0.1",
  appAuthors = ["Shane Stephens"],
  appShortDesc = "Togra examples",
  appProject = "Togra",
  appCmds = [sphereCmd, sphere2, sphere3, lineCmd, bezierCmd, bezier2,
	     bezierPatchCmd]
}

command name desc handler = defCmd {
  cmdName = name,
  cmdHandler = handler,
  cmdShortDesc = desc
}

sphereCmd = command "sphere" "Renders a sphere" (liftIO $
  togra 640 480 (tograInT 5 
	  (sphere 100 100 >>> (arr (\a -> (a,a))))
          Quads))

sphere2 = command "sphere2" "Renders an unoptimised MSP sphere" (liftIO $
  do
    putStrLn (show $ sphereMA' 100 100)
    togra 640 480 (tograInT 5 
      ((eval $ sphereMA' 100 100) >>> arr (\a -> (a,a))) 
      Quads))

sphere3 = command "sphere3" "Renders an optimised MSP sphere" (liftIO $
  do
    putStrLn (show $ sphereMA 100 100)
    togra 640 480 (tograMInT 5
      (sphereMA 100 100 >>> Arr (\a -> (a,a))) 
      Quads))

lineCmd = command "line" "Renders a line" (liftIO $
  togra 640 480 (tograMInT 5
	  (line 20 (Vertex3 0 0 0) (Vertex3 3 3 3) >>> Batch 20 >>> 
           Arr (\a -> (a,a))) 
	  LineStrip))

bezierCmd = command "bezier" "Renders a bezier" (liftIO $
  togra 640 480 (tograMInT 5
	  (In [0..19] >>> Arr (\a -> fromIntegral a / fromIntegral 20) >>>
	   bezier' [Vertex3 0 0 0, Vertex3 0 2 2, 
                    Vertex3 2 0 0, Vertex3 (-1) 1 1]
	   >>> Batch 20 >>> Arr (\a -> (a,a))) 
	  LineStrip))

bezier2 = command "bezier2" 
  "Renders a bezier using the fully generalised bezier function" (liftIO $ 
    do
	putStrLn (show theArr)
        togra 640 480 (tograMInT 5 theArr LineStrip))
    where
	theArr = ((aThenb [[Vertex3 0 0 0, Vertex3 0 2 2, 
		   Vertex3 2 0 0, Vertex3 (-1) 1 1]]
		  [fromIntegral x / fromIntegral 20 | x <- [0..19]]) >>>
	  bezier >>> Batch 20 >>> Arr (\a -> (a,a)))

bezierPatchCmd = command "bezierPatch" "Renders a bezier patch" (liftIO $
  do
	putStrLn (show theArr)
        togra 640 480 (tograMInT 5 theArr Quads))
  where
    -- TODO: Make divs a CL Arg (and other similar things elsewhere)
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
