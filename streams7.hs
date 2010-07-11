import Control.Monad

data SP m a b = Put b (m (SP m a b)) | Get (m a ->  m (SP m a b))

instance Category SP m where
  id			     = Get (liftM (\x -> Put x id))
  (Get msp2f) . (Put i msp1) = liftm (>>>) msp1 (msp2f (return i))
  (Put o msp2) . sp1         = Put o >>> 

-- this very quickly runs into problems - how can we combine monadic
-- processors back into non-monadic ones?
