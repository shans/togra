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

bezier' :: [Vertex3 Float] -> MSP Float (Vertex3 Float)
bezier' basis = Arr (bezier'')
  where
    bezier'' :: Float -> Vertex3 Float
    bezier'' s = bezier''' basis 0 s
    size = length basis
    bezier''' [] _ _ = Vertex3 0.0 0.0 0.0
    bezier''' (h:r) j t = (v3t (\a -> a * (t^j) * ((1-t)^(size-j-1)) * 
	     fromIntegral (binom j (size-1))) h) +!+ bezier''' r (j+1) t
    binom y x = div (prodxy y (x-1)) (prodxy 1 (x-y-1))
    prodxy 0 y = prodxy 1 y
    prodxy x y = product[x..y]


lift :: (a -> MSP b c) -> MSP (Either a b) c
lift f = left (Arr f) >>> App 

--  A generalized bezier that takes in a list of control points.
bezier :: MSP (Either [Vertex3 Float] Float) (Vertex3 Float)
bezier = lift bezier'

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

aThenb :: [a] -> [b] -> MSP c (Either a b)
aThenb a b = In [map toLeft a] >>> seqArr (map toRight b) >>> Unbatch id
