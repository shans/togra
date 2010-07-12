import SP
import Timing
import Togra
import TograStream

main = togra 640 480 (tograIn (
  left' (arr id) >>> (instrument "Sphere" $ left $ sphere 100 100) >>> collect (arr id) >>> (arr (\a -> (a,a)))))
