import Control.Arrow

constantVal :: Int

constantVal = 4

incrementingElement :: Int -> Int

incrementingElement = \a -> a + 1

addTwoElement :: (Int, Int) -> Int

addTwoElement = \(a, b) -> a + b

s = (first incrementingElement) >>> addTwoElement 

