module TograStream where

-- TODO: Move most of this out into stream utility files.

import Data.DateTime
import Graphics.Rendering.OpenGL
import Shader
import SP
import Togra
import Vbo

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

putL :: (Monad m) => [b] -> SP m a b -> SP m a b
putL [] r = r
putL (a:b) r = Put a (putL b r)

partOfAng x n ang = fromIntegral x * ang / fromIntegral n

circleGen :: (Monad m, Floating b) => Int -> SP m a (Vertex3 b)
circleGen n = putL [Vertex3 (cos x) (sin x) 0.0 | y <- [0..(n-1)], 
		      x <- [partOfAng y n (2.0 * pi)]] 
			  (circleGen n)

batch :: (Monad m) => Int-> SP m a [a]
batch n = batch' n [] (batch n)
batch' 0 l r = Put l r
batch' n l r = Get (\a -> batch' (n - 1) (a:l) r)

flatten :: (Monad m) => SP m [[a]] [a]
flatten = arr concat

sphereLineGen :: (Monad m, Floating b) => Int -> SP m a (Vertex3 b)
sphereLineGen n = putL [Vertex3 0.0 0.0 (cos (partOfAng x (n - 1) pi)) 
	    | x <- [0..(n-1)]] (sphereLineGen n) 

sphereSliceSizeGen :: (Monad m, Floating b) => Int -> SP m a b
sphereSliceSizeGen n = putL [sin (partOfAng x (n - 1) pi) | x <- [0..(n-1)]]
	(sphereSliceSizeGen n)

scaleExtrude :: (Monad m, Floating a) 
    => SP m ((Vertex3 a, a), [Vertex3 a]) [Vertex3 a]
scaleExtrude = arr (\((Vertex3 px py pz, scale), shape) ->
      map (\(Vertex3 x y z) -> Vertex3 (px+scale*x) (py+scale*y) (pz+scale*z))
	  shape)

pairwise :: Monad m => (a -> a -> b) -> SP m a b
pairwise f = Get (\a1 -> pairwise' f a1)
pairwise' f a1 = Get (\a2 -> Put (f a1 a2) (pairwise' f a2))

toQuads :: [a] -> [a] -> [a]
toQuads l1 l2 = concat (zipWith toQuad ll1 ll2)
  where
    ll1 = zip l1 ((tail l1) ++ [head l1])
    ll2 = zip l2 ((tail l2) ++ [head l2])
    toQuad (a,b) (c,d) = [a,b,d,c]

sphere :: (Monad m) => Int -> Int -> SP m i [Vertex3 Float]
sphere slices segments = ((sphereLineGen slices &&& sphereSliceSizeGen slices) &&& (circleGen segments >>> batch segments)) >>> scaleExtrude >>> (pairwise toQuads) >>> batch (slices - 1) >>> flatten

-- TODO: improve error message :)
checkMatches :: DataType -> VariableType -> IO ()
checkMatches Float FloatVec3 = return ()
checkMatches a b = do error ((show a) ++ " doesn't match " ++ (show b))

makeDataStreamInput :: (Show a, GlTypable a) => 
	ShaderTag -> BufferTarget -> BufferUsage -> [a] -> IO TograInput
makeDataStreamInput tag target usage l = do
  dvbo <- makeVBOWithData target usage l
  checkMatches (getVBOType dvbo) (getTagType tag)
  return $ DataStream tag dvbo

assocShaders :: (Show a, GlTypable a, Show b, GlTypable b) =>
    [ShaderTag] -> SP IO ([a],[b]) TograInput
assocShaders activeTags = Get (\(a, b) -> Block (
      do
	ti1 <- makeDataStreamInput tag1 ArrayBuffer StaticDraw a
	ti2 <- makeDataStreamInput tag2 ArrayBuffer StaticDraw b
	return $ Put ti1 (Put ti2 (assocShaders activeTags)))) where
  -- how can we make this dynamic?
  tag1:tag2:[] = activeTags

--tograIn :: SP IO () ([a], [b]) -> [ShaderTag] -> SP IO () TograInput
tograIn s t = s >>> (assocShaders t)
