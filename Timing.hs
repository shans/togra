module Timing where

import Data.DateTime
import SP
import TograUtil
import TograStream

data Tag = IGet | IPut | IBlock 
  deriving Show

data Instrumentation = I Tag String DateTime
  deriving Show

instrument :: String -> 
	      SP IO (Either a Instrumentation) (Either b Instrumentation) 
	      -> SP IO (Either a Instrumentation) (Either b Instrumentation)
instrument label (Put (Left a) s) = Block (do
  time <- getCurrentTime
  return (Put (Left a) (Put (Right (I IPut label time)) (instrument label s))))
instrument label (Get fs) = Get instrument'
  where
    instrument' (Left a) = Block (do
      time <- getCurrentTime
      return (Put (Right (I IGet label time)) (instrument label (fs (Left a)))))
    instrument' (Right i) = Put (Right i) (instrument label (Get fs)) 
instrument label (Block ms) = Block (do
  time <- getCurrentTime
  s <- ms
  return (Put (Right (I IBlock label time)) (instrument label s)))

left' :: Monad m => SP m a b -> SP m a (Either b c)
left' (Put a s) = Put (Left a) (left' s)
left' (Get f) = Get (\a -> left' (f a))
left' (Block ms) = Block (do
  s <- ms
  return (left' s))

display :: SP IO Instrumentation b
display = Get (\a -> Block (do
  putStrLn (show a)
  return display))

collect :: SP IO a b -> SP IO (Either a Instrumentation) b
collect f = f ||| display

fps :: Integer -> SP IO a a
fps duration = Block (do
  time <- getCurrentTime
  return (fps' duration time 0))
  where
    fps' duration start count = Block (do
      time <- getCurrentTime
      let endPeriod = addSeconds duration start
      let afterEnd = time > endPeriod
      if afterEnd 
	then putStrLn (show ((fromIntegral count) / (fromIntegral duration)))
	else return ()
      let next = fi afterEnd (fps' duration endPeriod 0) (fps' duration start (count + 1))
      return (Get (\a -> Put a next)))

tograInT period s = tograIn (s >>> fps period)

tograMInT period msp m t = tograMIn msp m t >>> fps period
