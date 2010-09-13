module Path where

import Graphics.Rendering.OpenGL
import VertexUtil
import SP
import SPUtil
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

binom x y = div (prodxy (y-x+1) y) (prodxy 1 x)
prodxy x y = product[x..y]

-- TODO: use non-function version of v3SymFun?
bezierF :: [Vertex3 Float] -> Float -> Vertex3 Float
bezierF basis s = bezier' basis 0 s
  where
    size = length basis
    bezier' [] _ _ = Vertex3 0.0 0.0 0.0
    bezier' (h:r) j t = (v3SymFunF (\a -> a * (t^j) * ((1-t)^(size-j-1)) * 
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

-- Provide each a as Left a .., then each b as Right b, unbatched.
aThenb :: [a] -> [b] -> MSP c (Either a b)
aThenb a b = In [map toLeft a] >>> seqArr (map toRight b) >>> Unbatch

-- Provide all of [a] as Left (Left a), then interleave each [b] 
-- (as Left (Right b)) with all of [c] (as Right c) each time. i.e.:
-- [a1,a2,a3] [b1,b2,b3] [c1,c2,c3] ->
-- [LLa1, LLa2, LLa3, 
--  LRb1, Rc1, Rc2, Rc3, 
--  LRb2, Rc1, Rc2, Rc3,
--  LRb3, Rc1, Rc2, Rc3]
aThenbThenc :: [a] -> [b] -> [c] -> MSP d (Either (Either a b) c)
aThenbThenc a b c = In [map toLeft (map toLeft a)] >>> 
    interArr (map toLeft (map toRight b)) (map toRight c) >>> Unbatch

bezierPatch = left (liftF bezierPatchF) >>> FApp

quadNormalF :: [Vertex3 Float] -> Vertex3 Float
quadNormalF (a:b:c:d:[]) = norm $ (c -!- a) -*- (b -!- d)

quadNormal = Arr quadNormalF

repl n = Arr (replicate n)

{- 
given a function, (a -> b -> c), what might we want to do with it?

we might wish to require a pair (a,b) and generate a c - i.e.
MSP (a,b) c

we might also wish to accept Left a values and Right b values, partially 
applying each (a) to generate (b -> c), and maintaining a "current" value.

Arr (left f) >>> FApp does this.  Arr (left f) takes Left a, generating Left
(b -> c), or Right d as a pass-through.  FApp takes Left (a -> b) or Right a,
storing Left (a -> b) and applying it to each subsequent Right a.

We can implement an MSP (a,b) c in terms of this:
Arr \(a, b) -> [Left a, Right b] >>> Unbatch >>> Arr (left f) >>> FApp.

We can also implement MSP (a,b) c directly:
Arr (\a,b) -> f a b

And we can implement MSP (Either a b) c in terms of MSP (a,b) c, although this
is less optimal because of possible recalculation of the partially evaluated
result.

BezierPatch is interesting because we do this:

[[Control Points]]     [0..1]
              \         /
               \       /
             [Control Points]    [0..1]
                      \            /
                       \         /
                     Resulting Point


When we implement an MSP (Either a b) c, it's generally because we want to 
provide an a, then run through a stream of b's to generate a stream of c's.

This is also true at this higher level, but the control is sort of backwards - 
we want to provide the [[CP]]s, then run through the values in [0..1], but 
FOR EACH VALUE, we want to provide the result as a [CP] and run through the 
values in [0..1] again.  So if we have an arrow that grabs an upstream, lefts
it, then provides a sequence of local results, we should be OK for this
use case.

Of course there's a more general problem if we want to treat the [0..1]
as streams too.

We could also have an arrow that takes (a,[b]) pairs, pushing out Left a
followed by each Right b?
-} 

stripe :: MSP (a,[b]) (Either a b)
stripe = Arr (\(a,bl) -> (Left a):(map toRight bl)) >>> Unbatch

appLTR :: MSP () a -> MSP () b -> Int -> (a -> b -> c) -> MSP () c
appLTR la ra bs f = (la &&& (ra >>> Batch bs)) >>> stripe >>> liftF f 

{-

So we start with a matrix (say 4x4) of points.  We need to generate a bezier
value at xs for each one.  Then we need to bundle that up into an array of
4 points, and stripe it to get a bezier curve.  We need to do this for
each xs. Woah - is this appRTL?

-}

bezierPatchA ctlPts ctlPtsLen xs xlen ys ylen = 
  appLTR (appLTR xs ctlPts ctlPtsLen (\a -> (\b -> bezierF b a)) >>> 
    Batch ctlPtsLen) ys ylen bezierF >>> Batch ylen >>> Batch xlen >>> 
      Arr (pairwiseL toQuads) >>> concatMA
