import SP
import Timing
import Togra
import TograStream
import MSP
import Graphics.Rendering.OpenGL

main = do
  putStrLn (show $ sphereMA 100 100)
  togra 640 480 (\t -> (tograMIn (sphereMA 100 100 >>> Arr (\a -> (a,a))) Quads t) >>> fps 5)
