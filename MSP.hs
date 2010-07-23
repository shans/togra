module MSP where

import Prelude hiding ( id, (.) )
import Control.Category
import Control.Arrow
import Graphics.Rendering.OpenGL
import SP
import SPUtil

data MSP a b where
    In :: [b] -> MSP a b
    Batch :: Int -> MSP a [a]
    Arr :: (a -> b) -> MSP a b
    First :: MSP a b -> MSP (a,c) (b,c)
    Dot :: MSP b c -> MSP a b -> MSP a c
    Par :: MSP a b -> MSP a c -> MSP a (b,c)
    ESP :: SP IO a b -> MSP a b

instance Show (MSP a b) where
  show (In v) = "|->"
  show (Arr f) = "->"
  show (First f) = "-l->"
  show (Dot b a) = (show a) ++ " >>> " ++ (show b)
  show (ESP a) = "-!->"
  show (Par a b) = "(" ++ show a ++ " &&& " ++ show b ++ ")"
  show (Batch n) = "-<" ++ (show n) ++ ">->"

split :: Int -> [a] -> [[a]]
split n [] = []
split n l = (take n l):(split n (drop n l))

times :: Int -> [a] -> [a]
times 0 l = []
times n l = l ++ times (n-1) l

modBatch :: Int -> [a] -> [[a]]
modBatch n l = split n (times m l)
  where
    m = div (lcm n (length l)) (length l)

instance Category MSP where
  id = Arr id
  (Arr g) . (Arr f) = Arr (g . f)
  (Arr f) . (In vl) = In (map f vl)
  (Batch n) . (In vl) = In (modBatch n vl)
  b . a = Dot b a

modAnd :: [a] -> [b] -> [(a, b)]
modAnd la lb = zip (times m la) (times n lb)
  where
    cm = lcm (length la) (length lb)
    m = div cm (length la)
    n = div cm (length lb)

instance Arrow MSP where
  arr f = Arr f
  first = First
  (In v1) &&& (In v2) = In (modAnd v1 v2)
  a &&& b = Par a b

eval :: MSP a b -> SP IO a b
eval (In vl) = rPutL vl
eval (Arr f) = arr f
eval (Dot b a) = (eval a) >>> (eval b)
eval (First a) = first (eval a)
eval (ESP a) = a
eval (Par a b) = (eval a) &&& (eval b)
eval (Batch n) = batch n

vertex3FunMA a b c = Arr (\v -> Vertex3 (a v) (b v) (c v))

circleGenMA' n = ESP (0 ..& (n-1)) >>> Arr (partOfAng (2 * pi) n) >>> 
	 	 vertex3FunMA cos sin (\a -> 0)
circleGenMA n = In [0..(n-1)] >>> Arr (partOfAng (2 * pi) n) >>> 
	        vertex3FunMA cos sin (\a -> 0)

concatMA = Arr concat

sphereLineGenMA' n = ESP (0 ..& (n-1)) >>> Arr (partOfAng pi (n - 1)) >>>
		     vertex3FunMA (\a -> 0) (\a -> 0) cos
sphereLineGenMA n = In [0..(n-1)] >>> Arr (partOfAng pi (n - 1)) >>>
		    vertex3FunMA (\a -> 0) (\a -> 0) cos

sphereSliceSizeGenMA' n = ESP (0 ..& (n-1)) >>> Arr (partOfAng pi (n - 1)) >>>
		 	  Arr sin
sphereSliceSizeGenMA n = In [0..(n-1)] >>> Arr (partOfAng pi (n - 1)) >>>
			 Arr sin

scaleExtrudeMA = Arr (\((Vertex3 px py pz, scale), shape) ->
    map (\(Vertex3 x y z) -> Vertex3 (px+scale*x) (py+scale*y) (pz+scale*z))
	shape)

sphereMA' :: Int -> Int -> MSP i [Vertex3 Float]
sphereMA' slices segments = 
  ((sphereLineGenMA' slices &&& sphereSliceSizeGenMA' slices) &&&
    (circleGenMA' segments >>> ESP (batch segments))) >>> scaleExtrudeMA >>>
      ESP (pairwise toQuads) >>> ESP (batch (slices - 1)) >>> concatMA

sphereMA :: Int -> Int -> MSP i [Vertex3 Float]
sphereMA slices segments = 
  ((((sphereLineGenMA slices &&& sphereSliceSizeGenMA slices) &&&
    (circleGenMA segments >>> Batch segments)) >>> scaleExtrudeMA) >>> 
      Batch (slices)) >>> Arr (pairwiseL toQuads) >>> concatMA
