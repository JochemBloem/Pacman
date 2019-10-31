module Queue where
    
    {-
        IMPORTANT NOTE:
        We did not write this queue ourselves, we modified the code found here:
        https://codereview.stackexchange.com/questions/207919/a-simple-queue-implementation-in-haskell
     -}
    data Queue a = EmptyQ | Value a (Queue a) deriving (Show, Eq, Read)

    enqueue :: a -> Queue a -> Queue a
    enqueue x EmptyQ          = Value x EmptyQ
    enqueue x (Value a queue) = Value a (enqueue x queue)

    dequeue :: Queue a -> (Maybe a, Queue a)
    dequeue EmptyQ          = (Nothing, EmptyQ)
    dequeue (Value a queue) = (Just a , queue)

    empty :: Queue a -> Bool
    empty EmptyQ = True
    empty _      = False