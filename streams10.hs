-- what does the Control.Arrow.SP package in streamproc do?

import SP
import Data.DateTime

putM :: (Monad m) => m b -> SP m a b -> SP m a b
putM mv r = Block (do
	  	     v <- mv
		     return (Put v r)) 

time = putM getCurrentTime time

disp :: (Show i) => SP IO i ()
disp = mapSP (\i -> 
	  do
	    putStrLn (show i)
	    return ())

timeDiffAsMillis :: DateTime -> DateTime -> Integer
timeDiffAsMillis a b = floor (((fromRational (toMJD b)) - 
  (fromRational (toMJD a))) * 100000000)

app' :: (b -> (c, SP a b c)) -> SP a b c
app' f = Get (\i -> Put (fst (f i)) (snd (f i))) 

timeCounter :: (Monad m) => SP m DateTime Integer
timeCounter = app' (\init -> (0, arr (timeDiffAsMillis init)))
-- timeCounter = Get (\init -> Put 0 (arr (timeDiffAsMillis init)))

testArrow = (time >>> timeCounter >>> disp)	

data (Num a) => Vector3 a = Vector3 (a, a, a) deriving Show

putL :: (Monad m) => [b] -> SP m a b -> SP m a b
putL [] r = r
putL (a:b) r = Put a (putL b r)

partOfAng x n ang = fromIntegral x * ang / fromIntegral n

circleGen :: (Monad m, Floating b) => Int -> SP m a (Vector3 b)
circleGen n = putL [Vector3 (cos x, sin x, 0.0) | y <- [0..(n-1)], 
		      x <- [partOfAng y n (2.0 * pi)]] 
			  (circleGen n)

batch :: (Monad m) => Int-> SP m a [a]
batch n = batch' n [] (batch n)
batch' 0 l r = Put l r
batch' n l r = Get (\a -> batch' (n - 1) (a:l) r)

sphereLineGen :: (Monad m, Floating b) => Int -> SP m a (Vector3 b)
sphereLineGen n = putL [Vector3 (0.0, 0.0, cos (partOfAng x (n - 1) pi)) 
	    | x <- [0..(n-1)]] (sphereLineGen n) 

sphereSliceSizeGen :: (Monad m, Floating b) => Int -> SP m a b
sphereSliceSizeGen n = putL [sin (partOfAng x (n - 1) pi) | x <- [0..(n-1)]]
	(sphereSliceSizeGen n)

scaleExtrude :: (Monad m, Floating a) 
    => SP m ((Vector3 a, a), [Vector3 a]) [Vector3 a]
scaleExtrude = arr (\((Vector3 (px,py,pz), scale), shape) ->
      map (\(Vector3 (x,y,z)) -> Vector3 (px+scale*x,py+scale*y,pz+scale*z))
	  shape)

sphere :: (Monad m, Floating a) => Int -> Int -> SP m i [[Vector3 a]]
sphere slices segments = ((sphereLineGen slices &&& sphereSliceSizeGen slices) &&& (circleGen segments >>> batch segments)) >>> scaleExtrude >>> batch slices
