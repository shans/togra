module RenderTree where

import Transform
import Togra
import TograStream
import SP
import SPUtil
import MSP
import Graphics.Rendering.OpenGL

data RenderTree where
  Collection :: [RenderTree] -> RenderTree
  Geom :: PrimitiveMode -> MSP a ([Vertex3 Float], [Vertex3 Float]) 
			-> RenderTree
  Transform :: MSP () TograMatrix -> RenderTree -> RenderTree

data TreeTask =
    AssocOnce PrimitiveMode [Vertex3 Float] [Vertex3 Float]
  | Assoc PrimitiveMode (SP IO () ([Vertex3 Float], [Vertex3 Float])) 
  | Trans (SP IO () TograMatrix)
  | UnTrans


data ActionList =
    PutP TograInput
  | Stream (SP IO () [TograInput])

instance Show ActionList where
  show (PutP ti) = "PutP " ++ show ti
  show (Stream s) = "Stream ..."

rt2tt (Geom mode (In [(v,n)])) = [AssocOnce mode v n]
rt2tt (Transform t rt) = (Trans (eval t)):(rt2tt rt) ++ [UnTrans]
rt2tt (Collection l) = concat $ map rt2tt l

makeDataStreamInputs tags sp = 
  sp >>> makeDSI'
  where
    makeDSI' = Get (\(a, b) -> Block (do
      ti1 <- makeDataStreamInput tag1 ArrayBuffer StaticDraw a
      ti2 <- makeDataStreamInput tag2 ArrayBuffer StaticDraw b
      return $ Put [ti1, ti2] makeDSI'))
    tag1:tag2:[] = tags
makeTransformInput sp = 
  sp >>> arr (\a -> [TransformUpdate a])

tograRenderTreeTaskListIn [] tags = (return [PutP End])
tograRenderTreeTaskListIn ((AssocOnce mode l1 l2):rest) tags 
  = do
      restList <- tograRenderTreeTaskListIn rest tags
      ti1 <- makeDataStreamInput tag1 ArrayBuffer StaticDraw l1
      ti2 <- makeDataStreamInput tag2 ArrayBuffer StaticDraw l2
      return $ (PutP ti1):(PutP ti2):(PutP (RenderPrimitive mode)):restList
    where tag1:tag2:[] = tags    
tograRenderTreeTaskListIn ((Assoc mode sp):rest) tags
  = do
      restList <- tograRenderTreeTaskListIn rest tags
      return $ (Stream (makeDataStreamInputs tags sp)):
	       (PutP (RenderPrimitive mode)):restList
tograRenderTreeTaskListIn ((Trans sp):rest) tags
  = do
      restList <- tograRenderTreeTaskListIn rest tags
      return $ (Stream (makeTransformInput sp)):restList
tograRenderTreeTaskListIn (UnTrans:rest) tags
  = do
      restList <- tograRenderTreeTaskListIn rest tags
      return $ (PutP Untransform):restList

-- We need to mix repeated inputs with genuine streams.  This is what's
-- in the ActionList:
-- PutP - this is an input that needs to be repeated
-- Stream s - this needs to be iterated each round of the ActionList and 
-- inserted in the right place.
--
-- left will put if available, then get and push through a left OR right value.
-- right will have the same effect - i.e. both bias towards a local result.
-- +++ is defined as (left f) >>> (right g) which means it will bias right.
--
-- if I have [Stream, PutP, Stream, PutP]
-- first we get [] --> (id, self)
-- PutP --> (id, put v self)
-- Stream --> (id +++ s, put v self ||| unbatch)

taggedArrow :: Eq c => SP IO () b -> c -> SP IO (Either c b) (Either c b)
taggedArrow sp t = Get (ta sp) 
  where
    ta sp (Left pt) = if t == pt then taa sp else Put (Left pt) (Get (ta sp))
    ta sp (Right v) = Put (Right v) (Get (ta sp))
    taa (Get spf) = error "only use taggedArrow with generators"
    taa (Put v sp) = Put (Right v) (Get (ta sp))
    taa (Block msp) = Block (do
	sp <- msp
	return $ taa sp)

genVals :: Int -> SP IO () Int
genVals n = rPutL [1 .. n]

consumeVals :: SP IO Int ()
consumeVals = Get (\a -> consumeVals)

taggedStreams :: [ActionList] -> Int -> 
		 SP IO (Either Int [TograInput]) (Either Int [TograInput])
taggedStreams [] n = taggedArrow (arr (\a -> [])) (-1)
taggedStreams ((Stream s):t) n = (taggedArrow s n) >>> taggedStreams t (n+1)
taggedStreams ((PutP i):t) n
   = (taggedArrow (rPutL [[i]])) n >>> taggedStreams t (n+1)

actionListIn :: IO [ActionList] -> SP IO () TograInput
actionListIn mlist = Block (do
  list <- mlist
  putStrLn (show list)
  return $ genVals (length list) >>> arr (\a -> Left a) >>> 
	   taggedStreams list 1 >>>  
	   left (consumeVals) >>> arr untagR >>> unbatch)
    where
      untagR (Right v) = v

rtIn tree tags = actionListIn (tograRenderTreeTaskListIn (rt2tt tree) tags)

{-
actionListIn :: IO [ActionList] -> MSP () TograInput
actionListIn mlist = ESP (Block (do
    list <- mlist
    let a = actionListFront list
    let b = actionListBack list (actionListBack list)
    return $ eval $ a >>> b))
      where
	actionListFront [] = Dot (Arr id) (Arr id) 
	actionListFront ((PutP i):r) = actionListFront r
	actionListFront ((Stream s):r) = makeStreamThing s (actionListFront r)
	makeStreamThing s (Dot b a) = 
	    Dot (b ||| Unbatch) (Arr (\a -> Left a) >>> (a +++ ESP s))
	actionListBack [] andThen = andThen
	actionListBack ((PutP i):r) andThen 
	  = makeStreamThing2 i (actionListBack r andThen)
	actionListBack ((Stream s):r) andThen 
	  = makeStreamThing3 (actionListBack r andThen)
	addPut v s = ESP (Put v (eval s))
	makeStreamThing2 i (Dot b a) = Dot (addPut i b) a
	makeStreamThing3 (Dot b a) 
	  = Dot (b ||| Unbatch) ((Arr (\x -> Left x)) >>> a)
-}
