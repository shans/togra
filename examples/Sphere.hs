import SP
import Timing
import Togra
import SPUtil
import Graphics.Rendering.OpenGL

main = togra 640 480 (tograInT 5 
	  (sphere 100 100 >>> (arr (\a -> (a,a))))
          Quads)
