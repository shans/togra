import SP
import Timing
import Togra
import TograStream
import MSP

main = do
  putStrLn (show $ sphereMA 100 100)
  togra 640 480 (tograIn (
    left' (arr id) >>> (instrument "Sphere" $ left $ eval $ sphereMA' 100 100) >>> collect (arr id) >>> (arr (\a -> (a,a)))))
