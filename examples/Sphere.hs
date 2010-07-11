import SP
import Togra
import TograStream

main = togra 640 480 (tograIn (sphere 100 100 >>> arr (\a -> (a,a))))
