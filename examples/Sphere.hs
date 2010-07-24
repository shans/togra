import SP
import Timing
import Togra
import TograStream

main = togra 640 480 (tograIn (sphere 100 100 >>> fps 5 >>> (arr (\a -> (a,a)))))
