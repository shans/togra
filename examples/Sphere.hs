import SP
import Timing
import Togra
import TograStream
import SPUtil
import Graphics.Rendering.OpenGL

main = togra 640 480 (tograIn (sphere 100 100 >>> fps 5 >>> (arr (\a -> (a,a)))) Quads)
