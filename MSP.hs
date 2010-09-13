module MSP where

import Prelude hiding ( id, (.) )
import Control.Category
import Control.Arrow
import Graphics.Rendering.OpenGL
import SP
import SPUtil
import System.IO.Unsafe

data MSP a b where
    In :: [b] -> MSP a b
    -- TODO: Consider adding a primitive which is a foo_plus_function, to
    -- eliminate the need for functions with FApp, Batch and Unbatch.
    App :: MSP (Either (MSP a b) a) b
    FApp :: MSP (Either (a -> b) a) b 
    Batch :: Int -> MSP a [a]
    Unbatch :: MSP [a] a
    Arr :: (a -> b) -> MSP a b
    First :: MSP a b -> MSP (a,c) (b,c)
    Dot :: MSP b c -> MSP a b -> MSP a c
    Par :: MSP a b -> MSP a c -> MSP a (b,c)
    ESP :: SP IO a b -> MSP a b

instance Show (MSP a b) where
  show (In v) = "|->"
  show (Arr f) = "->"
  show (First f) = "-(!F" ++ (show f) ++ "F!)->"
  show (Dot b a) =  "(" ++ (show a) ++ " >>> " ++ (show b) ++ ")"
  show (ESP a) = "-!->"
  show (Par a b) = "(" ++ show a ++ " &&& " ++ show b ++ ")"
  show (Batch n) = "-<" ++ (show n) ++ ">->"
  show (Unbatch) = "-><->"
  show (App) = "-O->"
  show (FApp) = "-(!)->"

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

fApp :: (a -> b) -> [Either (a -> b) a] -> [b]
fApp f [] = []
fApp f ((Right v):r) = (f v):(fApp f r)
fApp g ((Left f):r) = fApp f r

instance Category MSP where
  id = Arr id
  -- these are our rewrite rules.  These make MSPs more efficient by
  -- precalculating data arrays where possible.
  -- In [(a,b)] >>> First g = In [(a,b)] >>> (g *** id) 
  --     = (In [a] >>> g) *** In [b]
  c . (Par (In a) (In b)) = c . In (zip a b)
  (First g) . (In v) = Par (g . In (map fst v)) (In (map snd v))
  (Arr g) . (Arr f) = Arr (g . f)
  (Arr f) . (In vl) = In (map f vl)
  (Batch n) . (In vl) = In (modBatch n vl)
  (Unbatch) . (In vl) = In (concat vl)
  FApp . (In vl) = In (fApp (\a -> error "no function yet") vl)
  (Dot c b) . a = c . (b . a)
  -- TODO: Add collapse rule for Unbatch >>> Batch and Batch >>> Unbatch
  -- TODO: figure out if Lift can be collapsed, and how
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

toRight a = Right a
toLeft a = Left a

instance ArrowChoice MSP where
  left (Arr f) = Arr (either (toLeft . f) toRight)
  left v = ESP (left (eval v))
  right (Arr f) = Arr (either toLeft (toRight . f))

eval :: MSP a b -> SP IO a b
eval (In vl) = rPutL vl
eval (Arr f) = arr f
eval (Dot b a) = (eval a) >>> (eval b)
eval (First a) = first (eval a)
eval (ESP a) = a
eval (Par (In v1) (In v2)) = eval $ In (modAnd v1 v2)
eval (Par a b) = (eval a) &&& (eval b)
eval (Batch n) = batch n
eval (Unbatch) = unbatch
eval App = liftSP (\a -> eval a)
eval FApp = liftSP (\a -> arr a)

-- TODO: move a lot of this out of here and rename - the MSP variants
-- should be undecorated, and where required for demonstration purposes
-- the SP variants can even be reconstructed via eval.
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
      ESP (pairwise toQuadLoop) >>> ESP (batch (slices - 1)) >>> concatMA

-- extra parens needed because >>> associates RTL.  This means we have
-- In >>> Arr >>> Batch >>> Arr, but we need to collapse In >>> Arr first,
-- then In >>> Batch, then In >>> Arr.
-- 
-- I can't see how to do this without a BatchArr primitive.  However,
-- Batch >>> Arr and Arr >>> Batch would both collapse to this.  In fact, we
-- could replace the idea of Batch with BatchArr.
sphereMA :: Int -> Int -> MSP i [Vertex3 Float]
sphereMA slices segments = 
  ((sphereLineGenMA slices &&& sphereSliceSizeGenMA slices) &&&
    (circleGenMA segments >>> Batch segments)) >>> scaleExtrudeMA >>> 
      Batch slices >>> Arr (pairwiseL toQuadLoop) >>> concatMA

