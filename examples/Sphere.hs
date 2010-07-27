import SP
import Timing
import Togra
import TograStream
import SPUtil

main = togra 640 480 (tograIn (sphere 100 100 >>> fps 5 >>> (arr (\a -> (a,a)))))
