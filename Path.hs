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

bezier' :: [Vertex3 Float] -> MSP Int [Vertex3 Float]
bezier' basis = Arr (bezier'')
  where
    bezier'' :: Int -> [Vertex3 Float]
    bezier'' s = map (bezier''' basis 0) (inputs s)
    inputs s = [fromIntegral x / fromIntegral (s-1) | x <- [0..(s-1)]]
    size = length basis
    bezier''' [] _ _ = Vertex3 0.0 0.0 0.0
    bezier''' (h:r) j t = (v3t (\a -> a * (t^j) * ((1-t)^(size-j-1)) * 
	     fromIntegral (binom j (size-1))) h) +!+ bezier''' r (j+1) t
    binom y x = div (prodxy y (x-1)) (prodxy 1 (x-y-1))
    prodxy 0 y = prodxy 1 y
    prodxy x y = product[x..y]

--  A generalized bezier that takes in a list of control points.
bezier = Lift bezier'
      
