import SP
import Timing
import Togra
import TograStream
import MSP
import Graphics.Rendering.OpenGL

main = do
  putStrLn (show $ sphereMA' 100 100)
  togra 640 480 (tograInT 5 
    ((eval $ sphereMA' 100 100) >>> arr (\a -> (a,a))) 
    Quads)

