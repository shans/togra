import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad
import Data.DateTime

data SP a b = Put b (SP a b) | Get (a -> SP a b)
put = Put
get = Get

instance Category SP where
  id                      = Get (\x -> Put x id)
  (Get sp2) . (Put i sp1) = sp1 >>> sp2 i
  (Put o sp2) . sp1       = Put o (sp1 >>> sp2)
  (Get sp2) . (Get sp1)   = Get (\i -> sp1 i >>> Get sp2)

instance Arrow SP where
  arr f = Get (\x -> Put (f x) (arr f))
  first f = bypass [] f
    where
      bypass ds (Get f) = Get (\(b,d) -> bypass (ds ++ [d]) (f b))
      bypass (d:ds) (Put c sp) = Put (c,d) (bypass ds sp)
      bypass [] (Put c sp) = Get (\(b,d) -> Put (c,d) (bypass [] sp))
  -- without this partial definition, sp1 &&& sp2 is defined as
  -- arr (\a -> (a, a)) >>> (left a) >>> (right b), however (left a) requires
  -- a get for a value that will be passed through, provided to (right b), 
  -- then stored indefinitely
  (&&&) (Put o1 sp1) (Put o2 sp2) = Put (o1, o2) (sp1 &&& sp2)

incrementingElement :: SP Int Int
incrementingElement = arr (\x -> x + 1)

addTwoElement :: SP (Int, Int) Int
addTwoElement = arr (\(a, b) -> a + b)

s = (first incrementingElement) >>> addTwoElement

liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 f a b = (a &&& b) >>> arr (\(a, b) -> f a b)

fibs = put 0 fibs'
fibs' = put 1 (liftA2 (+) fibs fibs')

getOutput :: SP a b -> (b, SP a b)
getOutput (Put r sp) = (r, sp)
getOutput (Get _) = error "no outputs"

getOutputs n s = if (n == 0)
		 then ([], s)
		 else (h:t, s')
		   where 
		     (h, s'') = getOutput s
		     (t, s') = getOutputs (n - 1) s''


timeDiffAsMillis :: DateTime -> DateTime -> Integer
timeDiffAsMillis a b = floor (((fromRational (toMJD b)) - 
  (fromRational (toMJD a))) * 100000000)

time :: SP a (IO DateTime)
time = Put getCurrentTime time

--time' :: IO (SP a DateTime)
--time' = do
--	  t <- getCurrentTime
--	  sp <- time'
--	  return (Put t sp)
-- This doesn't work because you need to recurse infinitely on the
-- sp <- time' step before a single instance of time' can be returned.

timeCounter :: SP (DateTime) (Integer)
timeCounter = Get (\init -> Put 0 (timeCounter' init))
timeCounter' init = Get (\v -> 
		      Put (timeDiffAsMillis v init) (timeCounter' init))

timeCounter'' :: SP (IO DateTime) (IO Integer)
timeCounter'' = Get (\init -> Put (return 0) (timeCounter''' init))
timeCounter''' init = Get (\v ->
	    Put (liftM2 timeDiffAsMillis v init) (timeCounter''' init))
-- this doesn't work because we don't evaluate the monad until the last 
-- minute, so init and v are always equal.  In fact this indicates the entire
-- stream needs to be under monadic control, as we can't extract init out of
-- the context of the monad.

--liftSP :: (Monad m) => SP a b -> m (SP (m a) (m b))
--liftSP (Put a b) = do
--		     sp <- liftSP b
--		     return (Put (return a) sp)
--liftSP (Get f) = return (Get (\ma -> do
--				      a <- ma
--				      r <- (liftSP (f a))
--				      return r))
-- Doesn't work - the Get term is bogus.
-- a is of type a
-- r is of type SP (m a) (m b)
-- \ma -> ... return r is therefore of type
--     (m a) -> IO (SP (m a) (m b))
-- but it should be of type (m a) -> SP (m a) (m b)

--liftSP :: (Monad m) => SP a b -> SP (m a) (m b)
--liftSP (Put a b) = Put (return a) (liftSP b)
--liftSP (Get f) = Get (\ma -> do
--				a <- ma
--				return (liftSP (f a)))
-- This has the same problem - I need to return because I'm in a monad
-- context inside the function, but the return type of the function shouldn't
-- be a monad.  In both cases, because the input is a computation the returned
-- state machine must be a computation too, but I can't model that with these
-- simple state machines.

getIOOutput :: (Show b) => IO (SP a (IO b)) -> IO (b, SP a (IO b))
getIOOutput i =
  do
    (Put exp sp) <- i
    v <- exp
    putStrLn (show v)
    return (v, sp)
   
getIOOutputs n s = sequence [getIOOutput s | x <- [1..n]]

