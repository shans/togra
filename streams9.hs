-- What about explicitly modelling the different types of computation?

data SP a b = RecPut b | Rec (a -> b) | NonRec (a -> (b, SP a b))

time = RecPut getCurrentTime

timeCounter = NonRec (\init -> (0, timeCounter' init))
timeCounter' init = Rec (\v -> timeDiffAsMillis v init)
