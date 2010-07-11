-- In this approach we'll try building up streams from smaller streams

--class Primitive a

--instance Primitive Double
--instance Primitive Int
--instance Primitive String
--instance (Primitive a, Primitive b) => Primitive (a, b)
-- etc...

-- s -> i o is wrong here
-- s i -> o requires full specification of types, both in method signatures
--          and when using process (i.e. the same as no dep)
-- s o -> i behaves the same as s i -> o
class Element s i o 
 where
    process :: s -> i -> o

data Element0 a = SD0 a

instance Element (Element0 a) () a
  where
    process (SD0 a) _ = a

data Element1 i o = SD1 (i -> o)

instance Element (Element1 i o) i o
  where
    process (SD1 f) i = f i

data (Element s i o) => ElementN s i' i o = SDN (i' -> s)
-- why can't I do this?
-- data (Element s i o) => ElementN' s i' = SDN' (i' -> s)

instance (Element s i o) => Element (ElementN s i' i o) (i', i) o
  where
    process (SDN f) (i1, i2) = process (f i1) i2

constantElement = SD0 (7 :: Int)

incrementingElement = SD1 (\a -> a + (1 :: Int))

addTwoValuesElement :: ElementN (Element1 Int Int) Int Int Int
addTwoValuesElement = SDN (\a -> SD1 (\b -> a + b))

-- This is quite ugly in types.

addThreeValuesElement :: ElementN (ElementN (Element1 Int Int) Int Int Int) Int (Int, Int) Int
addThreeValuesElement = SDN (\a -> SDN (\b -> SD1 (\c -> a + b + c)))

-- Make that _really_ ugly!
-- can we do a type synonym?

type TwoElement i1 i2 o = ElementN (Element1 i2 o) i1 i2 o

addTwoValuesElement' :: (Num a) => TwoElement a a a 
addTwoValuesElement' = SDN (\a -> SD1 (\b -> a + b))

-- yes, although all inputs and outputs still need to be strictly specified.
-- what about getting rid of the Primitive requirement?
-- we can do this and things are neater, but we still need to strictly specify
-- the inputs and outputs.

-- Probably should check here that we can use IO Monad :)

-- maybe this isn't such a problem in usage?  Next we need to represent a 
-- processing tree. And it's here that we really run into trouble.  Don't
-- we need to fully encode the hierarchical type of the tree?  And how do 
-- we do that - don't we need every type variable specified at the class
-- level?

