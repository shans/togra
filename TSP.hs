module TSP where

import Prelude hiding ( id, (.) )
import Control.Category
import Control.Monad

-- Big Question: Do we need all of the M variants, or just MVal?  Things would
-- be a lot simpler without the rest of them.
--
-- Under what circumstances would we want i -> m o as opposed to i -> o?  This
-- would be when we don't know what monad action to perform until we examine
-- the input - is this important?
--
-- Little Question: If we have the M variants, should MPart and MPartVal be
-- monadic in the returned TSP m i o?  This would be most flexible but also
-- quite heavyweight.
data TSP m i o = Rec Int Int ([i] -> [o])
	       | Put o (TSP m i o)
	       | Get (i -> TSP m i o)
	       | Block (m (TSP m i o))

listdot :: [b -> c] -> [a -> b] -> [a -> c]
listdot = appmod (.)

-- apply the provided function to [a] and [b], repeating [a] and [b] as 
-- necessary until both end at the same time.
appmod :: (a -> b -> c) -> [a] -> [b] -> [c]
appmod f a b = appmod' f a b
  where
    appmod' f [] [] = []
    appmod' f (ah:al) (bh:bl) = (f ah bh):(appmod' f al bl)
    appmod' f [] bl = appmod' f a bl
    appmod' f al [] = appmod' f al b

appdot :: [a -> b] -> [a] -> [b]
appdot = appmod ($)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n l = (take n l):(splitEvery n (drop n l))

instance Monad m => Category (TSP m) where
  id = Rec [id]
  (Get sp2) . (Put i sp1) = sp1 >>> sp2 i
  (Put o sp2) . sp1       = Put o (sp1 >>> sp2)
  (Get sp2) . (Get sp1)   = Get (\i -> sp1 i >>> Get sp2)
  (Block spm) . sp        = Block (liftM (sp >>>) spm)
  sp . (Block spm)        = Block (liftM (>>> sp) spm)
  _ . (Rec insa 0 fa) = Rec insa 0 fa
  (Rec 0 outsb fb) . _ = Rec 0 outsb fb
  (Rec insb outsb fb) . (Rec insa outsa fa) = 
    Rec (insa * na) (outsb * nb) (rpt fa fb)
      where
	tlen = gcd outsa insb
        na = tlen / outsa
        nb = tlen / insb
    
      
      
