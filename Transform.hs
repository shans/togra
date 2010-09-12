module Transform where

import Graphics.Rendering.OpenGL

type TograMatrix = ((GLdouble, GLdouble, GLdouble, GLdouble),
		    (GLdouble, GLdouble, GLdouble, GLdouble),
		    (GLdouble, GLdouble, GLdouble, GLdouble),
		    (GLdouble, GLdouble, GLdouble, GLdouble))

translateM :: GLdouble -> GLdouble -> GLdouble -> TograMatrix
translateM x y z = ((1, 0, 0, x), (0, 1, 0, y), (0, 0, 1, z), (0, 0, 0, 1))

rotateM :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> TograMatrix
rotateM x y z r = ((x2+(1-x2)*c, x*y*(1-c)-z*s, x*z*(1-c)+y*s, 0),
		   (x*y*(1-c)+z*s, y2+(1-y2)*c, y*z*(1-c)-x*s, 0),
		   (x*z*(1-c)-y*s, y*z*(1-c)+x*s, z2+(1-z2)*c, 0),
		   (0, 0, 0, 1))
  where 
    x2 = x*x
    y2 = y*y
    z2 = z*z
    c = cos r
    s = sin r	

t :: TograMatrix -> TograMatrix
t ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) =
  ((a,e,i,m),(b,f,j,n),(c,g,k,o),(d,h,l,p))

toGLMatrix :: TograMatrix -> IO (GLmatrix GLdouble)
toGLMatrix ((a,b,c,d),(e,f,g,h),(i,j,k,l),(m,n,o,p)) =
  newMatrix RowMajor [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
