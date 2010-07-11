import Data.HList

constantElement :: HCons Int HNil
constantElement = 7 .*. hNil

addOffset :: HCons Int (HCons Int HNil) -> HCons Int HNil
addOffset (HCons a (HCons b hNil)) = (a + b) .*. hNil

f0 = Id

r0 = hFoldr f0 6 hNil

f1 :: (Int, Int) -> Int
f1 (a, b) = a + b

r1 :: Int
r1 = hFoldr f1 (6 :: Int) constantElement

f2 :: (String, (Int, Int)) -> (String, Int)
f2 (a, (b, c)) = (a, b + c)

--r2 :: (String, Int)
--r2 = hFoldr f2 (6 :: Int) ("foo" .*. (6 :: Int) .*. hNil)
-- Doesn't work because hFoldr requires a function that is generic enough
-- to accept each stage?  In this case it needs to take (String, r) -> o, and 
-- also (Int, Int) -> r.

-- I suspect that hMap will have the identical difficulties.  It seems there
-- really is no safe way to reason about these things without building
-- type classes around them :(

-- The next problem is a representation that works for streams at all. 
-- I'm going to switch to a restricted set of data and no HLists to see
-- if I can get that to work.
