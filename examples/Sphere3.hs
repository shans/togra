import SP
import Timing
import Togra
import TograStream
import MSP

main = do
  putStrLn (show $ sphereMA 100 100)
  togra 640 480 (tograIn ((eval $ sphereMA 100 100) >>> fps 5 >>> arr (\a -> (a,a))))
