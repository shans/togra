module SPUtil where

import Graphics.Rendering.OpenGL
import SP

-- TODO: Split into base utilities / geometry utilities / shapes
putL :: (Monad m) => [b] -> SP m a b -> SP m a b
putL [] r = r
putL (a:b) r = Put a (putL b r)

rPutL :: (Monad m) => [b] -> SP m a b
rPutL l = rPutL'
  where rPutL' = putL l rPutL'

partOfAng ang n x = fromIntegral x * ang / fromIntegral n

(..&) :: (Monad m) => Int -> Int -> SP m a Int
s ..& e = rPutL [s..e]

vertex3Fun :: (Monad m) => (a -> b) -> (a -> b) -> (a -> b) -> SP m a (Vertex3 b)
vertex3Fun a b c = arr (\v -> Vertex3 (a v) (b v) (c v))

circleGen :: (Monad m, Floating b) => Int -> SP m a (Vertex3 b)
circleGen n = 0 ..& (n-1) >>> arr (partOfAng (2 * pi) n) >>> 
	      vertex3Fun cos sin (\a -> 0)

batch :: (Monad m) => Int-> SP m a [a]
batch n = batch' n [] (batch n)
batch' 0 l r = Put l r
batch' n l r = Get (\a -> batch' (n - 1) (a:l) r)

concatA :: (Monad m) => SP m [[a]] [a]
concatA = arr concat

sphereLineGen :: (Monad m, Floating b) => Int -> SP m a (Vertex3 b)
sphereLineGen n = 0 ..& (n - 1) >>> arr (partOfAng pi (n - 1)) >>>
	          vertex3Fun (\a -> 0) (\a -> 0) cos

sphereSliceSizeGen :: (Monad m, Floating b) => Int -> SP m a b
sphereSliceSizeGen n = 0 ..& (n - 1) >>> arr (partOfAng pi (n - 1)) >>> arr sin

-- TODO: recast in terms of vertex algebra
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

pairwiseL :: (a -> a -> b) -> [a] -> [b]
pairwiseL f [a] = []
pairwiseL f (a:b:l) = (f a b):(pairwiseL f (b:l))

sphere :: (Monad m) => Int -> Int -> SP m i [Vertex3 Float]
sphere slices segments = ((sphereLineGen slices &&& sphereSliceSizeGen slices) &&& (circleGen segments >>> batch segments)) >>> scaleExtrude >>> (pairwise toQuads) >>> batch (slices - 1) >>> concatA

lift :: Monad m => (a -> SP m b c) -> SP m (Either a b) c
lift f = lift' onlyGet where
  lift' (Put v sp) = Put v (lift' sp)
  lift' sp = Get (lift'' sp)
  lift'' sp (Left a) = lift' (f a)
  lift'' (Get fsp) (Right b) = lift' (fsp b)
  onlyGet = Get (\_ -> onlyGet)

printSP :: Show a => SP IO a a
printSP = Get (\a -> Block(do
  putStrLn (show a)
  return (Put a printSP)))
