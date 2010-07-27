module TograStream where

import Data.DateTime
import Graphics.Rendering.OpenGL
import Shader
import SP
import Togra
import Vbo

-- TODO: Move this out of here
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

testArrow = (time >>> timeCounter >>> disp)	

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
