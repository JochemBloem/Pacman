module Types where
    {-
        MAZE
    -}
    type Maze  = [Field]
    data Field = Wall   | Spawn      | SpawnDoor | Item 
            deriving(Show, Eq)
    data Item  = Empty  | Dot        | Energizer | Fruit
    data Fruit = Cherry | Strawberry | Orange    | Apple | Melon | Galaxian | Bell | Key

    -- returns the size of the field 
    sizeF :: (Float, Float)
    sizeF =  (15.0 , 15.0 ) -- @TODO: make adjustable?
    sizeI :: (Int  , Int  )
    sizeI =  (15   , 15   )

    screenSizeF :: (Float, Float)
    screenSizeF = (600.0, 600.0)
    {-
        GAMESTATE
     -}
    data Gamestate = Gamestate  {   maze           :: Maze
                                ,   player         :: Pacman
                                ,   enemies        :: [Ghost]
                                ,   score          :: Int 
                                ,   level          :: Int
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
    data Ghost = Blinky Location Direction GhostBehaviour Int
               | Pinky  Location Direction GhostBehaviour Int
               | Inky   Location Direction GhostBehaviour Int
               | Clyde  Location Direction GhostBehaviour Int
          
    data GhostBehaviour = Chase | Frightened | Scatter

    {-
        TRANSLATION FUNCTIONS    
     -}
    locationToIndex :: Location -> Int
    locationToIndex (x,y) = floor (y * width + x)
                        where (width,_) = sizeF


    indexToLocation :: Int -> Location
    indexToLocation i     = (f x, f y)
                                where 
                                    x               = i `mod` width 
                                    y               = i `div` height
                                    f               = fromIntegral
                                    (width,height)  = sizeI