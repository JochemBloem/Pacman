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

    isOnTile :: Location -> Bool
    isOnTile (x,y) = abs (round' x - x) < 0.01 && abs (round' y - y) < 0.01

    getTargetTile :: Maze -> Location -> Direction -> Float -> TargetTile
    getTargetTile m (x,y) N v = (newLoc, getField m checkLoc)
        where 
            checkLoc          = roundLoc (x,             y + (0.5 + v))
            newLoc            = (x, y + v)
    getTargetTile m (x,y) E v = (newLoc, getField m checkLoc) 
        where 
            checkLoc          = roundLoc (x + (0.5 + v), y            )
            newLoc            = (x + v, y)
    getTargetTile m (x,y) S v = (newLoc, getField m checkLoc) 
        where 
            checkLoc          = roundLoc (x,             y - (0.5 + v))
            newLoc            = (x, y - v)
    getTargetTile m (x,y) W v = (newLoc, getField m checkLoc) 
        where 
            checkLoc          = roundLoc (x - (0.5 + v), y            )
            newLoc            = (x - v, y)
    
    shiftLocation :: Direction -> Location -> Float -> Location
    shiftLocation N (x,y) n = (x  , y+n)
    shiftLocation E (x,y) n = (x+n, y  )
    shiftLocation S (x,y) n = (x  , y-n)
    shiftLocation W (x,y) n = (x-n, y  )
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
    getGhostLocation (Blinky l _ _ _ _ _) = l
    getGhostLocation (Pinky  l _ _ _ _ _) = l
    getGhostLocation (Inky   l _ _ _ _ _) = l
    getGhostLocation (Clyde  l _ _ _ _ _) = l

    getGhostDirection :: Ghost -> Direction
    getGhostDirection (Blinky _ dir _ _ _ _) = dir
    getGhostDirection (Pinky  _ dir _ _ _ _) = dir
    getGhostDirection (Inky   _ dir _ _ _ _) = dir
    getGhostDirection (Clyde  _ dir _ _ _ _) = dir

    getGhostTime :: Ghost -> Float
    getGhostTime (Blinky _ _ _ t _ _) = t
    getGhostTime (Pinky  _ _ _ t _ _) = t
    getGhostTime (Inky   _ _ _ t _ _) = t
    getGhostTime (Clyde  _ _ _ t _ _) = t

    isNotScared :: Ghost -> Bool
    isNotScared g = getGhostBehaviour g /= Frightened

    getGhostBehaviour :: Ghost -> GhostBehaviour
    getGhostBehaviour (Blinky _ _ b _ _ _) = b
    getGhostBehaviour (Pinky  _ _ b _ _ _) = b
    getGhostBehaviour (Inky   _ _ b _ _ _) = b
    getGhostBehaviour (Clyde  _ _ b _ _ _) = b

    setGhostBehaviour :: GhostBehaviour -> Ghost -> Ghost
    setGhostBehaviour b (Blinky loc dir _ t i finit) = Blinky loc dir b t i finit
    setGhostBehaviour b (Pinky  loc dir _ t i finit) = Pinky  loc dir b t i finit
    setGhostBehaviour b (Inky   loc dir _ t i finit) = Inky   loc dir b t i finit
    setGhostBehaviour b (Clyde  loc dir _ t i finit) = Clyde  loc dir b t i finit

    
    isEaten :: Location -> Ghost -> Bool
    isEaten pacLoc g = pLoc == roundLoc gLoc && gb == Frightened
            where
                gb   = getGhostBehaviour g
                gLoc = roundLoc (getGhostLocation g)
                pLoc = roundLoc pacLoc
    
    frighten :: Ghost -> Float -> Ghost
    frighten (Blinky loc dir _ t i _) = Blinky loc dir Frightened t i
    frighten (Pinky  loc dir _ t i _) = Pinky  loc dir Frightened t i
    frighten (Inky   loc dir _ t i _) = Inky   loc dir Frightened t i
    frighten (Clyde  loc dir _ t i _) = Clyde  loc dir Frightened t i

    unFrighten :: Ghost -> Float -> Ghost
    unFrighten g@(Blinky loc dir gb t i finit) now | now >= finit + frightenedTimer = Blinky loc dir Chase t i 0
                                                   | otherwise                      = g    
    unFrighten g@(Pinky  loc dir gb t i finit) now | now >= finit + frightenedTimer = Pinky  loc dir Chase t i 0
                                                   | otherwise                      = g    
    unFrighten g@(Inky   loc dir gb t i finit) now | now >= finit + frightenedTimer = Inky   loc dir Chase t i 0
                                                   | otherwise                      = g    
    unFrighten g@(Clyde  loc dir gb t i finit) now | now >= finit + frightenedTimer = Clyde  loc dir Chase t i 0
                                                   | otherwise                      = g

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
    round' :: (RealFrac a, Num b) => a -> b
    round' = fromIntegral . round

    roundLoc :: Location -> Location
    roundLoc (x,y) = (round' x, round' y)

    distance :: Location -> Location -> Float
    distance (x1,y1) (x2,y2) = sqrt (f (x1-x2) + f (y1-y2))
                where
                    f :: Float -> Float
                    f n = n*n

    -- update the levelTimer in the gamestate
    updateLevelTimer :: Gamestate -> Float -> Gamestate
    updateLevelTimer gstate secs = gstate { levelTimer = levelTimer gstate + secs }