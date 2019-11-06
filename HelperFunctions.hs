module HelperFunctions where
    {-
        IMPORTS
     -}
    import Types
    import Data.List (sortBy)
    import Data.Function (on)
    import System.Random

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
    getGhostLocation (Blinky l _ _ _) = l
    getGhostLocation (Pinky  l _ _ _) = l
    getGhostLocation (Inky   l _ _ _) = l
    getGhostLocation (Clyde  l _ _ _) = l

    {-
        MAZE
     -}
    setMazeField :: Maze -> Location -> Field -> Maze
    setMazeField m loc f = take n m ++ [f] ++ drop (n + 1) m
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

    distance :: Location -> Location -> Float
    distance (x1,y1) (x2,y2) = sqrt (f (x1-x2) + f (y1-y2))
                where
                    f :: Float -> Float
                    f n = n*n

    getRandomNumber :: Int -> Int -> Int
    getRandomNumber upperLimit lowerLimit = lowerLimit -- @TODOJOCHEM: actueel willekeurig getal  