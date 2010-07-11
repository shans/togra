data Primitive = F1 Float 
	       | F2 Float Float
	       | F3 Float Float Float
	       | S1 String
-- etc..

-- constantElement :: Primitive
-- constantElement = F1 7.

--addOffset :: Primitive -> Primitive -> Primitive
-- This obviously isn't going to work because now we're just pretending
-- the types don't exist at all.

class PrimitiveData a

instance PrimitiveData Double

-- needs -XTypeSynonymInstances
instance PrimitiveData String
-- etc..

class Stream0 s a | s -> a
  where
    nextVal0 :: s -> (s, a)

data Stream0Data s a = S0D s (s -> (s, a))

instance (PrimitiveData a) => Stream0 (Stream0Data s a) a
  where
    nextVal0 (S0D s f) = (S0D s' f, a)
      where
	(s', a) = f s

constantElement = S0D () (\_ -> ((), 7.0))

class Stream1 s a b | s a -> b
  where
    nextVal1 :: s -> a -> (s, b)

data Stream1Data s a b = S1D s (s -> a -> (s, b))

instance (PrimitiveData a, PrimitiveData b) => Stream1 (Stream1Data s a b) a b
  where
    nextVal1 (S1D s f) a = (S1D s' f, b)
      where
	(s', b) = f s a

addFour = S1D () (\_ b -> ((), b + 4.0))

-- snd (nextVal1 addFour (3.0 :: Double))
-- works, but note the requirement to 'prove' that the input is a PrimitiveData.
-- we might be able to get rid of the PrimitiveData requirement, not sure.
-- At any rate, the nextVal0, nextVal1, etc. makes this impossible to automate
-- pipelines so I think it's dead in the water.
