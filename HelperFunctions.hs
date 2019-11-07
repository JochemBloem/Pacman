module HelperFunctions where
    {-
        IMPORTS
     -}
    import Types
    import Data.List (sortBy)
    import Data.Function (on)

    {-
        MOVABLE
    -}
    roundedLocation :: Location -> Location
    roundedLocation (x,y) = (round' x, round' y)

    isOnTile :: Location -> Bool
    isOnTile (x,y) = abs (round' x - x) < 0.01 && abs (round' y - y) < 0.01
    {-
        PACMAN
    -}
    pacmanAccessible :: Field -> Bool
    pacmanAccessible Wall      = False
    pacmanAccessible Spawn     = False
    pacmanAccessible SpawnDoor = False
    pacmanAccessible _         = True


    {-
        GHOSTS
    -}
    -- tells if a Ghost can walk a field
    ghostAccessible :: Field -> Bool
    ghostAccessible Wall = False
    ghostAccessible _    = True

    getGhostLocation :: Ghost -> Location
    getGhostLocation (Blinky l _ _ _ _) = l
    getGhostLocation (Pinky  l _ _ _ _) = l
    getGhostLocation (Inky   l _ _ _ _) = l
    getGhostLocation (Clyde  l _ _ _ _) = l

    setGhostDirection :: Direction -> Ghost -> Ghost
    setGhostDirection d (Blinky loc _ b t i) = Blinky loc d b t i
    setGhostDirection d (Pinky  loc _ b t i) = Pinky  loc d b t i
    setGhostDirection d (Inky   loc _ b t i) = Inky   loc d b t i
    setGhostDirection d (Clyde  loc _ b t i) = Clyde  loc d b t i

    getGhostTime :: Ghost -> Float
    getGhostTime (Blinky _ _ _ t _) = t
    getGhostTime (Pinky  _ _ _ t _) = t
    getGhostTime (Inky   _ _ _ t _) = t
    getGhostTime (Clyde  _ _ _ t _) = t

    isNotScared :: Ghost -> Bool
    isNotScared g = getGhostBehaviour g /= Frightened

    getGhostBehaviour :: Ghost -> GhostBehaviour
    getGhostBehaviour (Blinky _ _ b _ _) = b
    getGhostBehaviour (Pinky  _ _ b _ _) = b
    getGhostBehaviour (Inky   _ _ b _ _) = b
    getGhostBehaviour (Clyde  _ _ b _ _) = b

    setGhostBehaviour :: GhostBehaviour -> Ghost -> Ghost
    setGhostBehaviour b (Blinky loc dir _ t i) = Blinky loc dir b t i
    setGhostBehaviour b (Pinky  loc dir _ t i) = Pinky  loc dir b t i
    setGhostBehaviour b (Inky   loc dir _ t i) = Inky   loc dir b t i
    setGhostBehaviour b (Clyde  loc dir _ t i) = Clyde  loc dir b t i

    
    isEaten :: Location -> Ghost -> Bool
    isEaten pacLoc g = pacLoc == roundLoc gLoc && gb == Frightened
            where
                gb   = getGhostBehaviour g
                gLoc = roundLoc (getGhostLocation g)

    {-
        MAZE
     -}
    setField :: Maze -> Location -> Field -> Maze
    setField m loc f = take n m ++ [f] ++ drop (n + 1) m
                    where
                        n = locationToIndex loc

    getField :: Maze -> Location -> Field
    getField m l = m!!locationToIndex l 

    {-
        DIRECTION
    -}
    getDirection :: Location -> Location -> Direction
    getDirection (x1,y1) (x2,y2) = dir
                    where
                        dists       = [(N, y2-y1), (E, x2-x1), (S, y1-y2), (W, x1-x2)]
                        ((dir,_):_) = sortBy (flip compare `on` snd) dists -- list is hardcoded so cant go wrong
    

    oppositeDirection :: Direction -> Direction
    oppositeDirection N = S
    oppositeDirection E = W
    oppositeDirection S = N
    oppositeDirection W = E

    {-
        TRANSLATION FUNCTIONS    
     -}
    locationToIndex :: Location -> Int
    locationToIndex (x,y) = floor (y * width + x)
                        where (width,_) = boardSizeF


    indexToLocation :: Int -> Location
    indexToLocation i     = (f x, f y)
                                where 
                                    x               = i `mod` width 
                                    y               = i `div` height
                                    f               = fromIntegral
                                    (width,height)  = boardSizeI

    {-
        OTHER
    -}

    -- returns all items from list 1 that are not in list 2
    inverseList :: Eq a => [a] -> [a] -> [a]
    inverseList l  []    = l
    inverseList [] _     = []
    inverseList (x:xs) l | x `elem` l =     inverseList xs l
                         | otherwise  = x : inverseList xs l

    round' :: (RealFrac a, Num b) => a -> b
    round' = fromIntegral . round

    roundLoc :: Location -> Location
    roundLoc (x,y) = (round' x, round' y)

    distance :: Location -> Location -> Float
    distance (x1,y1) (x2,y2) = sqrt (f (x1-x2) + f (y1-y2))
                where
                    f :: Float -> Float
                    f n = n*n

    amountInList :: Eq a => a -> [a] -> Int
    amountInList _ []     = 0
    amountInList n (x:xs) | x == n    = 1 + amountInList n xs
                          | otherwise =     amountInList n xs