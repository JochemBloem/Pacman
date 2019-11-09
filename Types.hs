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
                                ,   levelTimer     :: Float
                                ,   saved          :: Bool
                                }
    data Status = Paused | GameOn | GameOver deriving (Eq)

    {-
        MOVABLE
     -}
    type Location   = (Float, Float)
    type TargetTile = (Location, Field)
    data Direction  = N | E | S | W deriving (Eq)
    {-
        CHARACTERS
     -}
    data Pacman = Pacman { location   :: Location
                         , direction  :: Direction
                         , lives      :: Int
                         , mouthangle :: Float
                         }
    data Ghost = Blinky Location Direction GhostBehaviour Float Int Float
               | Pinky  Location Direction GhostBehaviour Float Int Float
               | Inky   Location Direction GhostBehaviour Float Int Float
               | Clyde  Location Direction GhostBehaviour Float Int Float
            deriving (Eq)
          
    data GhostBehaviour = Chase | Frightened | Scatter
            deriving(Eq)
    
    {-
        CONSTANTS
     -}

    nO_SECS_BETWEEN_CYCLES :: Float
    nO_SECS_BETWEEN_CYCLES = 0.05
    
    pacmanSpeed :: Float
    pacmanSpeed = 0.125 -- 1 devided by pacmanspeed must be an integer

    ghostSpeed :: Float
    ghostSpeed = 0.1 -- for some reason the different speed causes weird non-fatal errors

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

    -- These durations for chase and scatter are not the official durations, 
    --  but they seemed to work better with our maze
    chaseTime :: Float
    chaseTime = 10

    scatterTime :: Float
    scatterTime = 5

    frightenedTimer :: Float
    frightenedTimer = 10

    blinkingTimer :: Float
    blinkingTimer = 4