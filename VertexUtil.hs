module VertexUtil where

import Graphics.Rendering.OpenGL
import Control.Arrow

v3Fun a b c = arr (\v -> Vertex3 (a v) (b v) (c v))

v3SymFunF :: (a -> b) -> Vertex3 a -> Vertex3 b
v3SymFunF f (Vertex3 a b c) = Vertex3 (f a) (f b) (f c)

v3SymFun f = arr $ v3SymFunF f

(+!+) :: Vertex3 Float -> Vertex3 Float -> Vertex3 Float
(Vertex3 a b c) +!+ (Vertex3 d e f) = Vertex3 (a+d) (b+e) (c+f)

(Vertex3 a b c) -!- (Vertex3 d e f) = Vertex3 (a-d) (b-e) (c-f)

(Vertex3 a b c) -*- (Vertex3 d e f) = Vertex3 (b*f-c*e) (c*d-a*f) (a*e-b*d)

a -.- (Vertex3 b c d) = Vertex3 (a*b) (a*c) (a*d)

norm (Vertex3 a b c) = Vertex3 (a/n) (b/n) (c/n)
  where
    n = sqrt (a*a + b*b + c*c)


