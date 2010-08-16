module Path where

import Graphics.Rendering.OpenGL
import SP
import MSP

class Interpolatable a where
  interp :: Float -> a -> a -> a

instance Interpolatable Float where
  interp f a b = a*f + b*(1-f)

instance Interpolatable a => Interpolatable (Vertex3 a) where
  interp f (Vertex3 sx sy sz) (Vertex3 ex ey ez) =
    Vertex3 (interp f sx ex) (interp f sy ey) (interp f sz ez)

interpolate :: (Interpolatable b) => Int -> b -> b -> MSP Int b 
interpolate size start end = Arr (\a -> interp (fromIntegral a / fromIntegral size) start end)

line :: Int -> Vertex3 Float -> Vertex3 Float -> MSP a (Vertex3 Float)
line pieces start end = In [0..(pieces-1)] >>> interpolate (pieces-1) start end

v3t :: (a -> b) -> Vertex3 a -> Vertex3 b
v3t f (Vertex3 a b c) = Vertex3 (f a) (f b) (f c)

(+!+) :: Vertex3 Float -> Vertex3 Float -> Vertex3 Float
(Vertex3 a b c) +!+ (Vertex3 d e f) = Vertex3 (a+d) (b+e) (c+f)

binom x y = div (prodxy (y-x+1) y) (prodxy 1 x)
prodxy x y = product[x..y]

bezierF :: [Vertex3 Float] -> Float -> Vertex3 Float
bezierF basis s = bezier' basis 0 s
  where
    size = length basis
    bezier' [] _ _ = Vertex3 0.0 0.0 0.0
    bezier' (h:r) j t = (v3t (\a -> a * (t^j) * ((1-t)^(size-j-1)) * 
	     fromIntegral (binom j (size-1))) h) +!+ bezier' r (j+1) t

bezierPatchF :: [[Vertex3 Float]] -> Float -> Float -> Vertex3 Float
bezierPatchF arr v1 v2 = bezierF (map (\a -> bezierF a v1) arr) v2

bezier' :: [Vertex3 Float] -> MSP Float (Vertex3 Float)
bezier' basis = Arr (bezierF basis)

lift :: (a -> MSP b c) -> MSP (Either a b) c
lift f = left (Arr f) >>> App 

liftF :: (a -> b -> c) -> MSP (Either a b) c
liftF f = Arr (left f) >>> FApp

--  A generalized bezier that takes in a list of control points.
bezier :: MSP (Either [Vertex3 Float] Float) (Vertex3 Float)
bezier = liftF bezierF

-- in order to use beziers and other lifted functions we need to be able to
-- sequence inputs.  We desire in [Left a, Left b, Left c, Right d, ...]
-- I'd like to express this as
-- seq (map toLeft [a,b,c]) >>> seq (map toRight [d,e,f]) >>> bezier
-- 
-- This would mean that seq takes all upstream inputs first, then all
-- local inputs.  However, this in turn means that seq only generates
-- a single set of inputs, not a loop of inputs like In.  So we actually
-- need seq to operate at the list level and only generate a single output:
-- In [map toLeft [a,b,c]] >>> seq (map toRight [d,e,f]) >>> unbatch >>> bezier

seqArr :: [a] -> MSP [a] [a]
seqArr e = Arr (\a -> a ++ e)

interArr :: [a] -> [a] -> MSP [a] [a]
interArr a b = Arr (\p -> p ++ interleave a b)
  where
    interleave [] _ = []
    interleave (h:t) b = (h:b) ++ interleave t b

aThenb :: [a] -> [b] -> MSP c (Either a b)
aThenb a b = In [map toLeft a] >>> seqArr (map toRight b) >>> Unbatch

aThenbThenc :: [a] -> [b] -> [c] -> MSP d (Either (Either a b) c)
aThenbThenc a b c = In [map toLeft (map toLeft a)] >>> 
    interArr (map toLeft (map toRight b)) (map toRight c) >>> Unbatch

bezierPatch = left (liftF bezierPatchF) >>> FApp

