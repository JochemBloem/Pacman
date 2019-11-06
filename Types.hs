module Types where
    {-
        MAZE
    -}
    type Maze  = [Field]
    data Field = Wall | Spawn | SpawnDoor | Empty | Dot | Energizer | Fruit 
            deriving(Show, Eq)

    
    {-
        GAMESTATE
     -}
    data Gamestate = Gamestate  {   maze           :: Maze
                                ,   player         :: Pacman
                                ,   enemies        :: [Ghost]
                                ,   newDir         :: Direction
                                ,   score          :: Int 
                                ,   level          :: Int
                                ,   elapsedTime    :: Float
                                ,   status         :: Status
                                }
    data Status = Paused | GameOn | GameOver

    {-
        MOVABLE
     -}
    type Location   = (Float, Float)
    type TargetTile = (Location, Field)
    data Direction  = N | E | S | W
    {-
        CHARACTERS
     -}
    data Pacman = Pacman { location  :: Location
                         , direction :: Direction
                         , lives     :: Int
                         }
    data Ghost = Blinky Location Direction GhostBehaviour Float
               | Pinky  Location Direction GhostBehaviour Float
               | Inky   Location Direction GhostBehaviour Float
               | Clyde  Location Direction GhostBehaviour Float
          
    data GhostBehaviour = Chase | Frightened | Scatter
            deriving(Eq)
    
    {-
        CONSTANTS
     -}

    nO_SECS_BETWEEN_CYCLES :: Float
    nO_SECS_BETWEEN_CYCLES = 0.05
     
    movableSpeed :: Float
    movableSpeed = 0.125 -- 1 devided by pacmanspeed must be an integer

    pixelsPerField :: Float
    pixelsPerField = 35

    boardSizeF :: (Float, Float)
    boardSizeF =  (15.0 , 15.0 ) -- @TODO: make adjustable?
    boardSizeI :: (Int  , Int  )
    boardSizeI =  (15   , 15   )

    screenSizeF :: (Float, Float)
    screenSizeF =  (600.0, 600.0)
    screenSizeI :: (Int, Int)
    screenSizeI =  (600, 600)

    defLoc :: Location
    defLoc = (-1,-1)
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
        HELPER FUNCTIONS
     -}

    roundedLocation :: Location -> Location
    roundedLocation (x,y) = (round' x, round' y)

    getGhostLocation :: Ghost -> Location
    getGhostLocation (Blinky l _ _ _) = l
    getGhostLocation (Pinky  l _ _ _) = l
    getGhostLocation (Inky   l _ _ _) = l
    getGhostLocation (Clyde  l _ _ _) = l

    setMazeField :: Maze -> Location -> Field -> Maze
    setMazeField m loc f = take n m ++ [f] ++ drop (n + 1) m
                    where
                        n = locationToIndex loc

    getField :: Maze -> Location -> Field
    getField m l = m!!locationToIndex l 

    round' :: (RealFrac a, Num b) => a -> b
    round' = fromIntegral . round

    isOnTile :: Location -> Bool
    isOnTile (x,y) = abs (round' x - x) < 0.01 && abs (round' y - y) < 0.01

    distance :: Location -> Location -> Float
    distance (x1,y1) (x2,y2) = sqrt (f (x1-x2) + f (y1-y2))
                where
                    f :: Float -> Float
                    f n = n*n