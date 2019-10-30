module Queue where
    
    import Types

    data Queue a = MkQueue [a]

    enqueue :: Queue a -> a -> Queue a
    enqueue = undefined

    dequeue :: Queue a -> (Queue a, Maybe a)
    dequeue = undefined

    type Location   = (Float, Float)