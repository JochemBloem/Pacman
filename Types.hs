module Types where
    {-
        MAZE
    -}
    type Maze  = [Field]
    data Field = Wall   | Spawn      | SpawnDoor | Item 
            deriving(Show, Eq)
    data Item  = Empty  | Dot        | Energizer | Fruit
    data Fruit = Cherry | Strawberry | Orange    | Apple | Melon | Galaxian | Bell | Key

    
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
     
    pacmanSpeed :: Float
    pacmanSpeed = 0.125 -- 1 devided by pacmanspeed must be an integer

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

    round' :: (RealFrac a, Num b) => a -> b
    round' = fromIntegral . round

    pacmanIsOnTile :: Location -> Bool
    pacmanIsOnTile (x,y) = abs (round' x - x) < 0.01 && abs (round' y - y) < 0.01