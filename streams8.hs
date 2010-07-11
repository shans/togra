-- let's try using a Kleisli arrow for IO and a Functor to add stream 
-- processing.

import Control.Arrow

newtype SPFunctor a b c = 
