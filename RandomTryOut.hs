module RandomTryOut where
    import Queue

    

    randInt :: Int
    randInt = v
        where
            (Just v, _) = dequeue randomQueue

            randomQueue :: Queue Int
            randomQueue = foldr enqueue EmptyQ [1,2,3]



    