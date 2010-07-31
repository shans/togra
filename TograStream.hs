module TograStream where

import Data.DateTime
import Graphics.Rendering.OpenGL
import Shader
import SP
import SPUtil
import MSP
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

-- builds a TograInput DataStream object from a ShaderTag and a buffer
makeDataStreamInput :: (Show a, GlTypable a) => 
	ShaderTag -> BufferTarget -> BufferUsage -> [a] -> IO TograInput
makeDataStreamInput tag target usage l = do
  dvbo <- makeVBOWithData target usage l
  checkMatches (getVBOType dvbo) (getTagType tag)
  return $ DataStream tag dvbo

clearDataStreamInput (DataStream tag dvbo) = freeDataFromVBO dvbo

-- TODO: rewrite this when shaders live outside of Togra's core.
-- Creates an SP that converts two lists into TograInput objects, based
-- around the default shader.
assocShaders :: (Show a, GlTypable a, Show b, GlTypable b) =>
    [ShaderTag] -> PrimitiveMode -> SP IO ([a],[b]) TograInput
assocShaders activeTags mode = Get (\(a, b) -> Block (
      do
	ti1 <- makeDataStreamInput tag1 ArrayBuffer StaticDraw a
	ti2 <- makeDataStreamInput tag2 ArrayBuffer StaticDraw b
	return $ putL [ti1, ti2, RenderPrimitive mode, End] 
		  (freeData ti1 ti2))) where
  -- how can we make this dynamic?
  tag1:tag2:[] = activeTags
  freeData ti1 ti2 = Block (do
    clearDataStreamInput ti1
    clearDataStreamInput ti2
    return $ assocShaders activeTags mode) 

assocShaderOnce :: (Show a, GlTypable a, Show b, GlTypable b) =>
    [ShaderTag] -> PrimitiveMode -> SP IO ([a],[b]) TograInput
assocShaderOnce activeTags mode = Get (\(a, b) -> Block (
      do
	ti1 <- makeDataStreamInput tag1 ArrayBuffer StaticDraw a
	ti2 <- makeDataStreamInput tag2 ArrayBuffer StaticDraw b
	return $ rPutL [ti1, ti2, RenderPrimitive mode, End])) where
  tag1:tag2:[] = activeTags

--tograIn :: SP IO () ([a], [b]) -> [ShaderTag] -> SP IO () TograInput
tograIn s m t = s >>> (assocShaders t m)

-- use an MSP and optimise if possible.
tograMIn (In l) m t = eval (In l) >>> (assocShaderOnce t m)
tograMIn msp m t = eval msp >>> (assocShaders t m)
