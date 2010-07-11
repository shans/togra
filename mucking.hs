newtype StateMonad s a = SM (s -> (a, s))

instance Monad (StateMonad s) where
  return a = SM (\s -> (a, s))
  x >>= f = SM (\s -> let SM x' = x
			  (a, s') = x' s
			  SM f' = f a
			  (b, s'') = f' s'
		      in (b, s''))

fetch :: StateMonad s s
fetch = SM (\s -> (s, s))

store :: s-> StateMonad s ()
store x = SM (\s -> ((), x))

tick :: StateMonad Int Int
tick =	fetch >>= \n ->
	store (n + 1) >>= \() ->
	return n

s :: StateMonad s Int
s = return 3

getItOut :: StateMonad s a -> s -> (a, s)
getItOut (SM f) s = f s

x = do
      a <- tick
      let a' = a * 2
      b <- tick
      let b' = b - 4
      c <- tick
      let c' = c * 5
      return (a' + b' + c')
