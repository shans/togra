import Data.HList


class HList l => HMapApply f l r | f l -> r
  where
    hMapApply :: f -> l -> r

instance HMapApply f HNil f
  where
    hMapApply f _ = f

class HApply f a b | f a -> b
  where
    hApply :: f -> a -> b

instance HApply (a -> b) a b
  where
    hApply f a = f a

instance (HMapApply f l r, HApply g e f) => HMapApply g (HCons e l) r
  where
    hMapApply g (HCons e l) = hMapApply (hApply g e) l

constantElement :: Int
constantElement = 7

addOffsetElement :: Int -> Int -> Int
addOffsetElement a b = a + b

data (HList a, HList b, HMap ) => Stream a b c d = AStream a (c -> d)

process :: (HList a, HList b) => Stream a b c d -> d
process (AStream l f) = hMapApply f (hMap process l) 
